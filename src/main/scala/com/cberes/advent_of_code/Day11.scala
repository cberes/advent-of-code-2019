package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.{State, toInput}
import com.cberes.advent_of_code.Day05.compute
import com.cberes.advent_of_code.Day09.computer

object Day11 {
  // computer input: 0 = black, 1 = white
  // computer output:
  //    1st value: 0 = black, 1 = white
  //    2nd value: 0 = 90* left, 1 = 90* right
  //    then move forward 1 place

  class Hull {
    private var hull = Map.empty[(Long, Long), Int]

    def apply(position : (Long, Long)) : Int = hull.getOrElse(position, 0)

    def paintedCount : Int = hull.size

    def paint(position : (Long, Long), value : Long) : Unit = {
      hull += (position -> value.toInt)
    }

    override def toString : String = {
      val xRange = hull.keys.map(_._1).min to hull.keys.map(_._1).max
      val yRange = hull.keys.map(_._2).min to hull.keys.map(_._2).max

      yRange.map(y => {
        xRange
          .map(x => (x, y))
          .map(hull.getOrElse(_, 0))
          .map(color => if (color == 0) ' ' else '#')
          .mkString
      }).mkString(System.lineSeparator())
    }
  }

  class Robot {
    private var position = (0L, 0L)
    private var direction = 0

    def where : (Long, Long) = position

    def turn(value : Long) : Unit = {
      val change = value match {
        case 0 => -90
        case 1 =>  90
      }

      direction = (direction + change) % 360
      direction = if (direction < 0) direction + 360 else direction
    }

    def move() : Unit = {
      position = direction match {
        case   0 => (position._1, position._2 - 1)
        case  90 => (position._1 + 1, position._2)
        case 180 => (position._1, position._2 + 1)
        case 270 => (position._1 - 1, position._2)
      }
    }
  }

  class HullController(hull : Hull, robot : Robot) {
    private var previousOutput : Option[Long] = None

    def yieldInput() : Long = hull(robot.where).toLong

    def acceptOutput(value : Long) : Unit = {
      previousOutput match {
        case None => previousOutput = Some(value)
        case Some(firstValue) =>
          hull.paint(robot.where, firstValue)
          robot.turn(value)
          robot.move()
          previousOutput = None
      }
    }
  }

  def paint(state : State, hull : Hull, robot : Robot) : Unit = {
    val controller = new HullController(hull, robot)
    compute(state, computer(controller.yieldInput, controller.acceptOutput))
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val hull = new Hull
    paint(state, hull, new Robot)
    hull.paintedCount
  }

  def part2(args : Array[String]) : String = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val hull = new Hull
    val robot = new Robot
    hull.paint(robot.where, 1)
    paint(state, hull, robot)
    hull.toString
  }
}
