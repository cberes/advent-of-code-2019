package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day14.{ComponentQuantity, Nanofactory, readInput}
import org.scalatest._

class Day14Spec extends FlatSpec with Matchers {
  private def oreRequired(resourceFile : String) : Int = {
    val factory = new Nanofactory(readInput(resourceIterator("day14/" + resourceFile)))
    factory.quantityRequired("ORE", ComponentQuantity("FUEL", 1))
  }

  private def fuelProduced(resourceFile : String) : Long = {
    val factory = new Nanofactory(readInput(resourceIterator("day14/" + resourceFile)))
    factory.totalProduced("ORE", ComponentQuantity("FUEL", 1), 1000000000000L)
  }

  "Day 14" can "find the ore for example 0" in {
    oreRequired("0.txt") shouldEqual 31
  }

  "Day 14" can "find the ore for example a" in {
    oreRequired("a.txt") shouldEqual 165
  }

  "Day 14" can "find the ore for example b" in {
    oreRequired("b.txt")shouldEqual 13312
  }

//  "Day 14" can "find the fuel for example b" in {
//    fuelProduced("b.txt") shouldEqual 82892753
//  }

  "Day 14" can "find the ore for example c" in {
    oreRequired("c.txt") shouldEqual 180697
  }

//  "Day 14" can "find the fuel for example c" in {
//    fuelProduced("c.txt") shouldEqual 5586022
//  }

  "Day 14" can "find the ore for example d" in {
    oreRequired("d.txt") shouldEqual 2210736
  }

  "Day 14" can "find the fuel for example d" in {
    fuelProduced("d.txt") shouldEqual 460664
  }
}
