package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.toInput
import com.cberes.advent_of_code.Day05.compute
import com.cberes.advent_of_code.Day09.computer

import scala.annotation.tailrec

object Day15 {
  private val directions = Map("N" -> 1L, "S" -> 2L, "W" -> 3L, "E" -> 4L)
  private val reverseDirections = Map("N" -> "S", "S" -> "N", "W" -> "E", "E" -> "W")

  abstract class Step {
    private var invalidSteps = Set[String]()

    def addInvalidStep(direction : String) : Unit =
      invalidSteps += direction

    protected def getInvalidSteps : Set[String] = invalidSteps

    def previousDirection : String

    def possibleSteps : Set[String]

    def undo : String

    def code : Long

    def tile(x : Int, y : Int) : (Int, Int)
  }

  class InitialStep extends Step {
    def possibleSteps : Set[String] = directions.keySet -- getInvalidSteps

    override def previousDirection: String = throw new IllegalStateException("initial step has no direction")

    def undo : String = throw new IllegalStateException("cannot undo initial step")

    def code : Long = throw new IllegalStateException("no code for initial step")

    override def tile(x : Int, y : Int) : (Int, Int) = (0, 0)
  }

  class Movement(val previousDirection : String) extends Step {
    def possibleSteps : Set[String] = directions.keySet -- getInvalidSteps - undo

    def undo : String = reverseDirections(previousDirection)

    def code : Long = directions(previousDirection)

    override def tile(x : Int, y : Int) : (Int, Int) = previousDirection match {
      case "N" => (x, y + 1)
      case "S" => (x, y - 1)
      case "E" => (x + 1, y)
      case "W" => (x - 1, y)
    }
  }

  class RepairDroid {
    private val Wall = 0L
    private val Moved = 1L
    private val Done = 2L

    private var walls = Set[(Int, Int)]()

    private var oxygen : Option[(Int, Int)] = None

    private var stepsToOxygen : List[Step] = Nil

    private var steps = List[Step](new InitialStep)

    private var lastDirection : String = _

    private var deadEnd = false

    def acceptOutput(value : Long) : Unit = value match {
      case Wall =>
        val (x, y) = visitedTiles.head
        walls += new Movement(lastDirection).tile(x, y)
        steps.head.addInvalidStep(lastDirection)
      case it =>
        if (deadEnd) {
          deadEnd = false
        } else {
          val nextStep = new Movement(lastDirection)
          steps = nextStep :: steps
        }

        if (it == Done) {
          oxygen = Some(visitedTiles.head)
          stepsToOxygen = steps
        }
    }

    def nextInput : Long = {
      val options = stepOptions

      if (options.isEmpty && steps.size <= 1) {
        // everything has been explored
        println(this)
        0
      } else if (options.isEmpty) {
        deadEnd = true
        val lastStep = steps.head
        steps = steps.tail
        steps.head.addInvalidStep(lastStep.previousDirection)
        directions(lastStep.undo)
      } else {
        lastDirection = options.head
        directions(lastDirection)
      }
    }

    private def stepOptions : Set[String] = {
      val visited = visitedTiles
      val visitedSet = visited.toSet

      steps.head.possibleSteps.filterNot(dir => {
        val step = new Movement(dir)
        val (x, y) = visited.head
        visitedSet.contains(step.tile(x, y))
      })
    }

    private def visitedTiles : List[(Int, Int)] = {
      steps.foldRight(List[(Int, Int)]())((step, acc) => {
        val next = if (acc.isEmpty) {
          (0, 0)
        } else {
          val (x, y) = acc.head
          step.tile(x, y)
        }
        next :: acc
      })
    }

    def stepCount : Int = steps.size - 1 // exclude initial step

    def stepsToOxygenCount : Int = stepsToOxygen.size - 1 // exclude initial step

    def timeToFillWithOxygen: Int =
      new Oxygen(walls, (oxygen.get._1, oxygen.get._2)).timeToFill

    override def toString : String = {
      val cells = visitedTiles.map(_ -> '.').toMap ++ walls.map(_ -> '#').toMap ++ oxygen.map(_ -> 'X').toMap

      val xRange = cells.keys.map(_._1).min to cells.keys.map(_._1).max
      val yRange = cells.keys.map(_._2).min to cells.keys.map(_._2).max

      yRange.map(y => {
        xRange
          .map(x => (x, y))
          .map(cells.getOrElse(_, ' '))
          .mkString
      }).mkString(System.lineSeparator())
    }
  }

  class Oxygen(walls : Set[(Int, Int)], start : (Int, Int)) {
    def timeToFill : Int = timeToFillRecursive(Map.empty, (start._1, start._2, 0) :: Nil)

    @tailrec
    private def timeToFillRecursive(visited : Map[(Int, Int), Int], remaining : List[(Int, Int, Int)]) : Int = {
      remaining match {
        case Nil => visited.values.max
        case (x, y, time) :: rest =>
          val next = neighbors(x, y)
            .filterNot(visited.contains)
            .filterNot(walls.contains)
            .map(pos => (pos._1, pos._2, time + 1))

          timeToFillRecursive(visited + ((x, y) -> time), rest ::: next)
      }
    }

    private def neighbors(x : Int, y : Int) : List[(Int, Int)] =
      (x, y + 1) :: (x, y - 1) :: (x + 1, y) :: (x - 1, y) :: Nil
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val droid = new RepairDroid
    compute(state, computer(() => droid.nextInput, droid.acceptOutput))
    droid.stepsToOxygenCount
  }

  def part2(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val droid = new RepairDroid
    compute(state, computer(() => droid.nextInput, droid.acceptOutput))
    droid.timeToFillWithOxygen
  }
}
