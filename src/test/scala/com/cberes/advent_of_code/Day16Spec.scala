package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day16.{intList, realSignal, signal}
import org.scalatest._

class Day16Spec extends FlatSpec with Matchers {
  "Day 16" can "do part 1" in {
    signal((1 to 8).toList, 4) shouldEqual List(0,1,0,2,9,4,9,8)
    signal(intList("80871224585914546619083218645595"), 100).take(8) shouldEqual List(2,4,1,7,6,1,7,6)
    signal(intList("19617804207202209144916044189917"), 100).take(8) shouldEqual List(7,3,7,4,5,4,1,8)
    signal(intList("69317163492948606335995924319873"), 100).take(8) shouldEqual List(5,2,4,3,2,1,3,3)
  }

  "Day 16" can "do part 2" in {
    realSignal(intList("03036732577212944063491565474664"), 100, 10000).take(8) shouldEqual List(8,4,4,6,2,0,2,6)
    realSignal(intList("02935109699940807407585447034323"), 100, 10000).take(8) shouldEqual List(7,8,7,2,5,2,7,0)
    realSignal(intList("03081770884921959731165446850517"), 100, 10000).take(8) shouldEqual List(5,3,5,5,3,7,3,1)
  }
}
