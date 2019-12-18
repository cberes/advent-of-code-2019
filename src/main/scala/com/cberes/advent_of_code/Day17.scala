package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.toInput
import com.cberes.advent_of_code.Day05.compute
import com.cberes.advent_of_code.Day09.computer

object Day17 {
  def intersections(s : String) : List[(Int, Int)] = {
    val lines = s.split('\n')
    (1 until lines.length - 1)
      .flatMap(y => (1 until lines(y).length - 1).map(x => (x, y)))
      .filter(p => lines(p._2)(p._1) == '#' &&
        lines(p._2 + 1)(p._1) == '#' &&
        lines(p._2 - 1)(p._1) == '#' &&
        lines(p._2)(p._1 + 1) == '#' &&
        lines(p._2)(p._1 - 1) == '#')
      .toList
  }

  /*
   * #  scaffold
   * .  open space
   * ^  vacuum robot (up)
   * v  vacuum robot (down)
   * >  vacuum robot (right)
   * <  vacuum robot (left)
   * X  vacuum robot (in space)
   */
  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val output = new StringBuilder
    compute(state, computer(() => 0, c => output.append(c.toChar)))
    println(output)
    intersections(output.toString).map(p => p._1 * p._2).sum
  }

  /*
   * Main function: A,A,B,C,B,C,B,C\n
   * Movement functions (x 3): 10,L,8,R,6\n
   * Video feed: y\n or n\n
   *
   * Functions can contain at most 20 characters + newline
   *
   * When robot has followed the program, it will output the amount of dust
   */
  def part2(args : Array[String]) : Long = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val robotAwake = state.copy(memory = state.memory + (0L -> 2L))
    val functions = "A,A,B,C,B,C,B,C,A,C\nR,6,L,8,R,8\nR,4,R,6,R,6,R,4,R,4\nL,8,R,6,L,10,L,10\nn\n"
    var input = 0
    var output = 0L
    compute(robotAwake, computer(() => {
      input += 1
      functions(input - 1).toLong
    }, dust => output = dust))
    output
  }
}
