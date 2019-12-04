package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day04.{isValid, isValidPart2}
import org.scalatest._

class Day04Spec extends FlatSpec with Matchers {
  "Day 4" should "solve part 1" in {
    isValid("111111") shouldEqual true
    isValid("223450") shouldEqual false
    isValid("123789") shouldEqual false
  }

  "Day 4" should "solve part 2" in {
    isValidPart2("112233") shouldEqual true
    isValidPart2("123444") shouldEqual false
    isValidPart2("111122") shouldEqual true
  }
}
