package com.cberes.advent_of_code

import scala.annotation.tailrec

object Day12 {
  case class Planet(position : Map[String, Int], velocity : Map[String, Int]) {
    def applyGravity(that : Planet) : Planet = {
      copy(velocity = position.map(p => {
        val change =
          if (p._2 > that.position(p._1)) -1
          else if (p._2 < that.position(p._1)) 1
          else 0

        p._1 -> (velocity(p._1) + change)
      }))
    }

    def updatePosition() : Planet =
      copy(position = position.map(p => p._1 -> (velocity(p._1) + p._2)))

    def totalEnergy : Int =
      position.map(_._2.abs).sum * velocity.map(_._2.abs).sum
  }

  def readPlanets(lines : Iterator[String]) : List[Planet] = {
    readPositions(lines).map(position => Planet(position, position.keys.map(_ -> 0).toMap))
  }

  def readPositions(lines : Iterator[String]) : List[Map[String, Int]] = {
    lines.map(s => s.substring(s.indexOf('<') + 1, s.indexOf('>')))
      .map(_.split("\\s*,\\s*"))
      .map(ss => ss.map(_.split("=", 2)).map(pair => pair(0) -> pair(1).toInt).toMap)
      .toList
  }

  def applyGravity(planets : List[Planet]) : List[Planet] = {
    planets.map(p => planets.filterNot(p.eq)
      .foldLeft(p)((acc, other) => acc.applyGravity(other)))
  }

  def updatePositions(planets : List[Planet]) : List[Planet] =
    planets.map(_.updatePosition())

  def totalEnergy(planets : List[Planet]) : Int =
    planets.map(_.totalEnergy).sum

  @tailrec
  def timeStep(planets : List[Planet], stepsRemaining : Int) : List[Planet] = {
    if (stepsRemaining == 0) {
      planets
    } else {
      val gravityApplied = applyGravity(planets)
      val positionUpdated = updatePositions(gravityApplied)
      timeStep(positionUpdated, stepsRemaining - 1)
    }
  }

  // Step is really the index of the computation that this function will perform.
  // Why then must it start at 2? I am puzzled.
  // But no one on Reddit has complained about this, so I assume I'm wrong.
  @tailrec
  def findRepeats(originalPlanets : List[Planet], planets : List[Planet], repeats : Map[String, Int] = Map.empty, step : Int = 2) : Map[String, Int] = {
    if (repeats.size == planets.head.position.size) {
      repeats
    } else {
      val gravityApplied = applyGravity(planets)
      val positionUpdated = updatePositions(gravityApplied)

      val newRepeats = planets.head.position.keys
        .filterNot(repeats.contains)
        .filter(key => (originalPlanets, positionUpdated).zipped.forall((a, b) => a.position(key) == b.position(key)))
        .map(_ -> step)
        .toMap

      findRepeats(originalPlanets, positionUpdated, repeats ++ newRepeats, step + 1)
    }
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val planets = readPlanets(lines)
    val laterPlanets = timeStep(planets, 1000)
    totalEnergy(laterPlanets)
  }

  def part2(args : Array[String]) : Map[String, Int] = doWithLines(args.head) { lines =>
    val planets = readPlanets(lines)
    val repeats = findRepeats(planets, planets)
    println("Now find the LCM of these numbers: " + repeats.values.mkString(" "))
    repeats
  }
}
