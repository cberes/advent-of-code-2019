package com.cberes.advent_of_code

import org.scalatest._

class Day01Spec extends FlatSpec with Matchers {
  "Day 1" should "solve part 1" in {
    Day01.fuel(12) shouldEqual 2
    Day01.fuel(14) shouldEqual 2
    Day01.fuel(1969) shouldEqual 654
    Day01.fuel(100756) shouldEqual 33583
  }

  "Day 1" should "solve part 2" in {
    Day01.totalFuel(14) shouldEqual 2
    Day01.totalFuel(1969) shouldEqual 966
    Day01.totalFuel(100756) shouldEqual 50346
  }
}
