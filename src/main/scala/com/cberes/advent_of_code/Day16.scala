package com.cberes.advent_of_code

import scala.annotation.tailrec

object Day16 {
  def realSignal(input : List[Int], phases : Int, repeat : Int) : List[Int] = {
    /*
     * Considering the offset and the total (repeated) signal length,
     * most of the signal can be ignored, because it's multiplied by 0.
     * The rest of the signal is multiplied by 1.
     * Because the latter inputs of the signal will be multiplied by 1,
     * we can save a lot of time by iterating through elements in reverse
     * order and adding the current element to the previous element.
     * (The very last element remains unchanged the entire time.)
     *
     * Of course, I didn't figure this out. Instead I copied the solution from
     * https://github.com/ednl/aoc2019/blob/master/16/day16b.c
     * And a kind soul posted this https://imgur.com/wAJ1zEj,
     * in which the shortcut is visualized.
     */
    val offset = input.take(7).mkString.toInt
    val inputSize = input.size
    val repeatedSignal = List.tabulate(repeat * inputSize)(i => input(i % inputSize))

    // use an array, because its elements are mutable and it has fast random access
    val relevantSignal = repeatedSignal.drop(offset).toArray
    val relevantSize = relevantSignal.length

    (0 until phases).foldLeft(relevantSignal)((signal, _) => {
      (relevantSize - 2 to 0 by -1).foreach(i => {
        signal(i) = (signal(i) + signal(i + 1)) % 10
      })
      signal
    }).toList
  }

  @tailrec
  def signal(input : List[Int], phases : Int) : List[Int] = {
    if (phases == 0) {
      input
    } else {
      signal(nextPhase(input), phases - 1)
    }
  }

  def nextPhase(input : List[Int]) : List[Int] = {
    input.indices
      .map(index => input.zipWithIndex.map(x => applyPattern(x._1, x._2, index)).sum.abs % 10)
      .toList
  }

  private def applyPattern(value : Int, inputIndex : Int, outputIndex : Int) : Int = {
    val maxIndex = (outputIndex + 1) * 4

    val i = (inputIndex + 1) % maxIndex

    if (i >= outputIndex + 1 && i < (outputIndex + 1) * 2) {
      value
    } else if (i >= (outputIndex + 1) * 3) {
      -value
    } else {
      0
    }
  }

  def intList(s : String) : List[Int] = s.toList.map(_ - '0')

  def part1(args : Array[String]) : String = doWithLines(args.head) { lines =>
    val input = intList(lines.next())
    signal(input, 100).take(8).mkString
  }

  def part2(args : Array[String]) : String = doWithLines(args.head) { lines =>
    val input = intList(lines.next())
    realSignal(input, 100, 10000).take(8).mkString
  }
}
