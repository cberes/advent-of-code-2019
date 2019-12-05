package com.cberes.advent_of_code

import scala.annotation.tailrec

object Day03 {
  type Position = (Int, Int)

  type State = Map[Position, Map[Int, Int]]

  case class Where(direction : Char, distance : Int)

  class Input(input : String) {
    def toPath : Seq[Where] = input.split(',').map(it => Where(it.charAt(0), it.substring(1).toInt))
  }

  implicit def toInput(input : String) : Input = new Input(input)

  implicit def toPath(input : String) : Seq[Where] = input.toPath

  class Coord(position : Position) {
    def manhattanDistance : Int = Math.abs(position._1) + math.abs(position._2)
  }

  implicit def toCoord(position : Position) : Coord = new Coord(position)

  class XState(state : State) {
    def closestIntersection : (Int, Int) = {
      val allWires = state.values.flatMap(_.keys).toSet

      state
        .filterNot(_._1 == (0,0))
        .filter(_._2.size == allWires.size)
        .keys
        .minBy(_.manhattanDistance)
    }

    def minStepsToIntersection : Int = {
      val allWires = state.values.flatMap(_.keys).toSet

      state
        .filterNot(_._1 == (0,0))
        .filter(_._2.size == allWires.size)
        .values
        .map(_.values.sum)
        .min
    }
  }

  implicit def toXState(state : State) : XState = new XState(state)

  private def emptyState : State = Map.empty

  def solve(wires : List[Seq[Where]]) : State =
    wires.zipWithIndex.foldRight(emptyState) { (path, state) => compute(path._1, path._2 + 1, 0, (0, 0), state) }

  @tailrec
  def compute(path : Seq[Where], key : Int, initialSteps : Int, position : Position, state : State) : State = {
    path.headOption match {
      case None => state
      case Some(it) =>
        val positions = it.direction match {
          case 'R' => (1 to it.distance).map(x => (position._1 + x, position._2))
          case 'L' => (1 to it.distance).map(x => (position._1 - x, position._2))
          case 'U' => (1 to it.distance).map(y => (position._1, position._2 + y))
          case 'D' => (1 to it.distance).map(y => (position._1, position._2 - y))
        }

        val nextState = state ++ positions.zipWithIndex
          .map(it => (it._1, it._2 + 1 + initialSteps))
          .map(it => {
            val currentStateValue = state.getOrElse(it._1, Map.empty)
            val nextStateValue = if (currentStateValue.contains(key)) currentStateValue else currentStateValue + (key -> it._2)
            it._1 -> nextStateValue
          })

        compute(path.tail, key, initialSteps + positions.size, positions.last, nextState)
    }
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) {
    lines => solve(lines.toList.map(_.toPath)).closestIntersection.manhattanDistance
  }

  def part2(args : Array[String]) : Int = doWithLines(args.head) {
    lines => solve(lines.toList.map(_.toPath)).minStepsToIntersection
  }
}
