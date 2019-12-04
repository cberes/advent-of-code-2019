package com.cberes.advent_of_code

import org.scalatest._
import com.cberes.advent_of_code.Day02.{compute, toInput, toState}

class Day02Spec extends FlatSpec with Matchers {
  "Day 2" can "create its state" in {
    "1,0,0,0,99".toState shouldEqual Map(0 -> 1, 1 -> 0, 2 -> 0, 3 -> 0, 4 -> 99)
  }

  "Day 2" should "solve part 1" in {
    compute("1,0,0,0,99") shouldEqual "2,0,0,0,99".toState
    compute("2,3,0,3,99") shouldEqual "2,3,0,6,99".toState
    compute("2,4,4,5,99,0") shouldEqual "2,4,4,5,99,9801".toState
    compute("1,1,1,4,99,5,6,0,99") shouldEqual "30,1,1,4,2,5,6,0,99".toState
    compute("1,9,10,3,2,3,11,0,99,30,40,50") shouldEqual "3500,9,10,70,2,3,11,0,99,30,40,50".toState
  }
}
