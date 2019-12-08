package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day06.{dijkstra, orbitCount, spaceMap}
import org.scalatest._

class Day06Spec extends FlatSpec with Matchers {
  private val spaceText = "COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L"
  private val lines = spaceText.split("\\s")
  private val graph = (spaceText + "\nK)YOU\nI)SAN").split("\\s")

  "Day 6" can "get orbit count for single planets" in {
    val space = spaceMap(lines.iterator)
    orbitCount(space, "L") shouldEqual 7
    orbitCount(space, "K") shouldEqual 6
    orbitCount(space, "J") shouldEqual 5
    orbitCount(space, "E") shouldEqual 4
    orbitCount(space, "D") shouldEqual 3
    orbitCount(space, "C") shouldEqual 2
    orbitCount(space, "B") shouldEqual 1
    orbitCount(space, "F") shouldEqual 5
    orbitCount(space, "I") shouldEqual 4
    orbitCount(space, "H") shouldEqual 3
    orbitCount(space, "G") shouldEqual 2
    orbitCount(space, "COM") shouldEqual 0
  }

  "Day 6" can "get orbit count for space" in {
    val space = spaceMap(lines.iterator)
    orbitCount(space) shouldEqual 42
  }

  "Day 6" can "find cheapest path" in {
    val space = spaceMap(graph.iterator, bidirectional = true)
    val vertices = dijkstra(space, "YOU")
    vertices("SAN").cost shouldEqual 6
  }
}
