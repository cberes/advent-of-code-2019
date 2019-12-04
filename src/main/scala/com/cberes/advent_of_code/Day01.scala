package com.cberes.advent_of_code

import scala.annotation.tailrec

object Day01 {
  private def sum(masses : Iterator[Int], f : Int => Int) : Int = masses.map(f).sum

  def fuel(mass : Int) : Int = Math.floor(mass / 3.0).toInt - 2

  def totalFuel(mass : Int) : Int = totalFuel(mass, 0)

  @tailrec
  def totalFuel(mass : Int, acc : Int) : Int = {
    val result = fuel(mass)
    if (result > 0) {
      totalFuel(result, acc + result)
    } else {
      acc
    }
  }

  def part1() : Int = doWithLines(inputDir + "day01.txt") {
    lines => sum(lines.map(_.toInt), fuel)
  }

  def part2() : Int = doWithLines(inputDir + "day01.txt") {
    lines => sum(lines.map(_.toInt), totalFuel)
  }
}
