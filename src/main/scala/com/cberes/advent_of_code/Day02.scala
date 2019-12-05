package com.cberes.advent_of_code

import scala.annotation.tailrec

object Day02 {
  class Input(input : String) {
    def toState : Map[Int, Int] = input.split(',').map(_.toInt).zipWithIndex.map(_.swap).toMap
  }

  implicit def toInput(input : String) : Input = new Input(input)

  implicit def toState(input : String) : Map[Int, Int] = input.toState

  @tailrec
  def compute(state : Map[Int, Int], position : Int = 0) : Map[Int, Int] = {
    state(position) match {
      case 99 => state

      case code if code == 1 || code == 2 => {
        val positionLeft = state(position + 1)
        val positionRight = state(position + 2)
        val positionOutput = state(position + 3)

        val left = state.getOrElse(positionLeft, 0)
        val right = state.getOrElse(positionRight, 0)
        val value = if (code == 1) left + right else left * right

        compute(state + (positionOutput -> value), position + 4)
      }
    }
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) {
    lines => compute(lines.next().toState ++ Map(1 -> args(1).toInt, 2 -> args(2).toInt))(0)
  }

  def part2(args : Array[String]) : Any = {
    for {noun <- 0 to 99; verb <- 0 to 99; if part1(Array(args.head, noun.toString, verb.toString)) == args.last.toInt} yield 100 * noun + verb
  }
}
