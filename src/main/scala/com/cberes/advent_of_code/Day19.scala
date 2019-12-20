package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.{State, toInput}
import com.cberes.advent_of_code.Day05.compute
import com.cberes.advent_of_code.Day09.computer

import scala.annotation.tailrec

object Day19 {
  def points(max : (Int, Int)) : List[(Int, Int)] =
    (0 until max._2).flatMap(y => (0 until max._1).map(x => (x, y))).toList

  class MapBuilder(points : List[(Int, Int)]) {
    var remainingPoints : List[(Int, Int)] = points.tail
    var currentPoint : (Int, Int) = points.head
    var results : Map[(Int, Int), Int] = Map.empty
    var sentX = false

    def xyz : Long = {
      val n = nextInput
      println(n)
      n
    }

    def nextInput : Long = {
      if (sentX) {
        currentPoint._2
      } else {
        sentX = true
        currentPoint._1
      }
    }

    def acceptOutput(o : Long) : Unit = {
      results += currentPoint -> o.toInt
      if (remainingPoints.nonEmpty) {
        sentX = false
        currentPoint = remainingPoints.head
        remainingPoints = remainingPoints.tail
      }
    }

    def tractorBeamMap : Map[(Int, Int), Int] = results
  }

  @tailrec
  def buildMap(state : State, points : List[(Int, Int)], acc : Map[(Int, Int), Int]) : Map[(Int, Int), Int] = {
    points match {
      case Nil => acc
      case point :: rest =>
        val entry = point -> beamAtPoint(state, point)
        buildMap(state, rest, acc + entry)
    }
  }

  def beamAtPoint(state : State, point : (Int, Int)) : Int = {
    var sentX = false
    var output = 0L
    compute(state, computer(() => {
      if (sentX) {
        point._2
      } else {
        sentX = true
        point._1
      }
    }, output = _))
    output.toInt
  }

  def mkMapString(beam : Map[(Int, Int), Int]) : String = {
    val xRange = beam.keys.map(_._1).min to beam.keys.map(_._1).max
    val yRange = beam.keys.map(_._2).min to beam.keys.map(_._2).max

    yRange.map(y => {
      xRange
        .map(x => (x, y))
        .map(beam)
        .map {
          case 0 => '.'
          case 1 => '#'
        }
        .mkString
    }).mkString(System.lineSeparator())
  }

  def angle(point : (Int, Int)) : Double =
    math.atan(point._2.toDouble / point._1.toDouble)

  @tailrec
  def findClosestPoint(
      state : State,
      offsets : List[(Int, Int)],
      bounds : (Double, Double),
      point : (Int, Int) = (1, 1)) : (Int, Int) = {
    if (angle(point) < bounds._1) {
      findClosestPoint(state, offsets, bounds, (1, point._2 + 1))
    } else if (angle(point) > bounds._2) {
      findClosestPoint(state, offsets, bounds, (point._1 + 1, point._2))
    } else if (offsets.forall(offset => beamAtPoint(state, (point._1 + offset._1, point._2 + offset._2)) == 1)) {
      point
    } else {
      findClosestPoint(state, offsets, bounds, (point._1 + 1, point._2))
    }
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val tractorBeamMap = buildMap(state, points((50, 50)), Map.empty)
    println(mkMapString(tractorBeamMap))
    tractorBeamMap.count(_._2 == 1)
  }

  def part2(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val size = 100
    val offsets = (0, 0) :: (0, size - 1) :: (size - 1, 0) :: (size - 1, size - 1) :: Nil
    // found these points from part 1
    val bounds = (angle((97, 71)), angle((97, 88)))
    val closest = findClosestPoint(state, offsets, bounds)
    closest._1 * 10000 + closest._2
  }
}
