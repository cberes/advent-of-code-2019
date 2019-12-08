package com.cberes.advent_of_code

import scala.collection.mutable

object Day06 {
  case class Orbit(orbits : String, around : String)

  case class Vertex(node : String, cost : Int)

  type SpaceMap = Map[String, Set[String]]

  def orbitCount(spaceMap: SpaceMap) : Int = {
    spaceMap.keys.foldRight(0)((key, acc) => acc + orbitCount(spaceMap, key))
  }

  def orbitCount(spaceMap : SpaceMap, planet : String) : Int = {
    spaceMap.get(planet) match {
      case Some(orbits) => orbits.map(key => 1 + orbitCount(spaceMap, key)).sum
      case None => 0
    }
  }

  def spaceMap(lines : Iterator[String], bidirectional : Boolean = false) : SpaceMap = {
    val orbits = lines
      .map(_.split("\\)"))
      .map(pair => Orbit(pair(1), pair(0)))

    orbits.foldRight(Map[String, Set[String]]()) { (orbit, map) =>
      val Orbit(a, b) = orbit
      val aValues = map.getOrElse(a, Set()) + b
      val bValues = map.getOrElse(b, Set()) + a
      val vertices = if (bidirectional) Map(a -> aValues, b -> bValues) else Map(a -> aValues)
      map ++ vertices
    }
  }

  def dijkstra(graph : SpaceMap, source : String) : Map[String, Vertex] = {
    val unvisited : mutable.Set[String] = mutable.Set()
    val distance : mutable.Map[String, Vertex] = mutable.Map()

    for (v <- graph.keys) {
      distance += (v -> Vertex(null, Int.MaxValue))
      unvisited += v
    }

    distance += (source -> Vertex(null, 0))

    while (unvisited.nonEmpty) {
      val u = unvisited.minBy(distance(_).cost)

      unvisited -= u

      for (v <- graph.getOrElse(u, Set.empty) if unvisited.contains(v)) {
        val alt = distance(u).cost + 1
        if (alt < distance(v).cost) {
          distance += (v -> Vertex(u, alt))
        }
      }
    }

    distance.toMap
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val space = spaceMap(lines)
    orbitCount(space)
  }

  def part2(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val space = spaceMap(lines, bidirectional = true)
    val vertices = dijkstra(space, "YOU")
    vertices("SAN").cost - 2
  }
}
