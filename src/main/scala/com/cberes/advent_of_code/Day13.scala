package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.toInput
import com.cberes.advent_of_code.Day05.compute
import com.cberes.advent_of_code.Day09.computer

import scala.collection.mutable

object Day13 {
  def toTiles(inputs : List[Long]) : Map[(Int, Int), Int] = {
    inputs.grouped(3).map(triple => (triple.head.toInt, triple(1).toInt) -> triple(2).toInt).toMap
  }

  class GameController {
    private var x : Option[Long] = None
    private var y : Option[Long] = None
    private var ball = (0L, 0L)
    private var paddle = (0L, 0L)
    var lastScore = 0L

    def acceptOutput(value : Long) : Unit = {
      if (x.isEmpty) {
        x = Some(value)
      } else if (y.isEmpty) {
        y = Some(value)
      } else {
        handleValue(value)
        x = None
        y = None
      }
    }

    private def handleValue(value : Long) : Unit = {
      if (x.get == -1 && y.get == 0) {
        lastScore = value
      } else {
        value match {
          case 3 => paddle = (x.get, y.get)
          case 4 => ball = (x.get, y.get)
          case _ =>
        }
      }
    }

    def nextInput : Long = (ball._1 - paddle._1).sign
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val inputs = new mutable.Queue[Long]
    inputs.enqueueAll(args.tail.map(_.toLong))
    val outputs = new mutable.ListBuffer[Long]
    compute(state, computer(inputs.dequeue, outputs.append))
    toTiles(outputs.toList).count(_._2 == 2)
  }

  def part2(args : Array[String]) : Long = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val withQuarters = state.copy(memory = state.memory + (0L -> 2L))
    val controller = new GameController
    compute(withQuarters, computer(() => controller.nextInput, controller.acceptOutput))
    controller.lastScore
  }
}
