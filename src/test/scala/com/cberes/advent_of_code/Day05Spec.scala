package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.toInput
import com.cberes.advent_of_code.Day05.parameterGetter
import org.scalatest._

class Day05Spec extends FlatSpec with Matchers {
  "Parameter getter" can "get parameters" in {
    parameterGetter("1,3,50,51,99".toState, 1) shouldEqual 51
    parameterGetter("1001,3,50,51,99".toState, 1) shouldEqual 51
    parameterGetter("1001,3,50,51,99".toState, 2) shouldEqual 50
  }

  "Day 5" can "read opcodes" in {
    Day05.opcode("1,3,50,51,99".toState) shouldEqual 1
    Day05.opcode("1001,3,50,51,99".toState) shouldEqual 1
  }
}
