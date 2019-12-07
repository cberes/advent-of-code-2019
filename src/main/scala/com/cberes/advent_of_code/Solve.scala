package com.cberes.advent_of_code

object Solve extends App {
  val solvers : Map[(Int, Int), Array[String] => Any] = Map(
    (1, 1) -> Day01.part1,
    (1, 2) -> Day01.part2,
    (2, 1) -> Day02.part1,
    (2, 2) -> Day02.part2,
    (3, 1) -> Day03.part1,
    (3, 2) -> Day03.part2,
    (4, 1) -> Day04.part1,
    (4, 2) -> Day04.part2,
    (5, 1) -> Day05.part1
  )

  val solver = solvers((args(0).toInt, args(1).toInt))

  val result = solver(args.drop(2))

  println(result)
}
