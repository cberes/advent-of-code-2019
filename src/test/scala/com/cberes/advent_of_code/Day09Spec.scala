package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.{toInput, State}
import com.cberes.advent_of_code.Day05.compute
import com.cberes.advent_of_code.Day09.computer
import org.scalatest._

import scala.collection.mutable

class Day09Spec extends FlatSpec with Matchers {
  "Day 9" can "solve example 1" in {
    val outputs = new mutable.ListBuffer[Long]
    val input = List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99)
    compute(input.mkString(",").toState, computer(() => 0, outputs.append))
    outputs.toList shouldEqual input
  }

  "Day 9" can "solve example 2" in {
    val outputs = new mutable.ListBuffer[Long]
    compute("1102,34915192,34915192,7,4,7,99,0".toState, computer(() => 0, outputs.append))
    outputs.toList.head.toString.length shouldEqual 16
  }

  "Day 9" can "solve example 3" in {
    val outputs = new mutable.ListBuffer[Long]
    compute("104,1125899906842624,99".toState, computer(() => 0, outputs.append))
    outputs.toList.head shouldEqual 1125899906842624L
  }

  "Day 9" can "suck it" in {
    val memory = Map(0L -> 109L, 1L -> 2000L, 2L -> 109L, 3L -> 19L, 4L -> 203L, 5L -> -34L, 6L -> 204L, 7L -> -34L, 8L -> 99L)
    val outputs = new mutable.ListBuffer[Long]
    val finalState = compute(State(memory), computer(() => 1, outputs.append))
    finalState.memory(1985L) shouldEqual 1
    finalState.relativeBase shouldEqual 2019
    outputs.toList.head shouldEqual 1
  }
}
