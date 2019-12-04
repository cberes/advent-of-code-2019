package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day03.{Where, solve, toCoord, toInput, toPath, toXState}
import org.scalatest._

class Day03Spec extends FlatSpec with Matchers {
  "Day 3" can "create its state" in {
    "R8,U5,L5,D3".toPath shouldEqual Seq(Where('R', 8), Where('U', 5), Where('L', 5), Where('D', 3))
  }

  "Day 3" should "solve part 1" in {
    solve(List("R8,U5,L5,D3", "U7,R6,D4,L4")).closestIntersection.manhattanDistance shouldEqual 6
    solve(List("R75,D30,R83,U83,L12,D49,R71,U7,L72", "U62,R66,U55,R34,D71,R55,D58,R83"))
      .closestIntersection.manhattanDistance shouldEqual 159
    solve(List("R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51", "U98,R91,D20,R16,D67,R40,U7,R15,U6,R7"))
      .closestIntersection.manhattanDistance shouldEqual 135
  }
}
