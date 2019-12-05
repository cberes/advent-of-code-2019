package com.cberes.advent_of_code

object Day04 {
  def isValid(password : String) : Boolean = {
    val hasRepeat = (0 until password.length - 1).exists(i => password.charAt(i) == password.charAt(i + 1))

    hasRepeat && characterValuesDoNotDecrease(password)
  }

  def isValidPart2(password : String) : Boolean = {
    val hasDoubleOnly = (0 until password.length - 1).exists(i => {
      password.charAt(i) == password.charAt(i + 1) &&
        (i == 0 || password.charAt(i) != password.charAt(i - 1)) &&
        (i == password.length - 2 || password.charAt(i) != password.charAt(i + 2))
    })

    hasDoubleOnly && characterValuesDoNotDecrease(password)
  }

  private def characterValuesDoNotDecrease(password : String) =
    (0 until password.length - 1).forall(i => password.charAt(i) <= password.charAt(i + 1))

  private def toRange(input : String) = {
    val bounds = input.split("-")
    (bounds(0).toInt to bounds(1).toInt)
  }

  def part1(args : Array[String]) : Int = toRange(args.head).map(_.toString).count(isValid)

  def part2(args : Array[String]) : Int = toRange(args.head).map(_.toString).count(isValidPart2)
}
