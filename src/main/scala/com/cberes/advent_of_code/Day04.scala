package com.cberes.advent_of_code

object Day04 {
  def isValid(password : String) : Boolean = {
    val hasRepeat = (0 until password.length - 1).exists(i => password.charAt(i) == password.charAt(i + 1))

    val increases = (0 until password.length - 1).forall(i => password.charAt(i) <= password.charAt(i + 1))

    hasRepeat && increases
  }

  def isValidPart2(password : String) : Boolean = {
    val hasDoubleOnly = (0 until password.length - 1).exists(i => {
      password.charAt(i) == password.charAt(i + 1) &&
        (i == 0 || password.charAt(i) != password.charAt(i - 1)) &&
        (i == password.length - 2 || password.charAt(i) != password.charAt(i + 2))
    })

    val increases = (0 until password.length - 1).forall(i => password.charAt(i) <= password.charAt(i + 1))

    hasDoubleOnly && increases
  }

  def part1() : Int = (125730 to 579381).map(_.toString).count(isValid)

  def part2() : Int = (125730 to 579381).map(_.toString).count(isValidPart2)
}
