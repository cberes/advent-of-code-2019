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

  def part1(noun : Int = 12, verb : Int = 2) : Int = doWithLines(inputDir + "day02.txt") {
    lines => compute(lines.next().toState ++ Map(1 -> noun, 2 -> verb))(0)
  }

  def part2() : Any = {
    for {noun <- 0 to 99; verb <- 0 to 99; if part1(noun, verb) == 19690720} yield 100 * noun + verb
  }
}
