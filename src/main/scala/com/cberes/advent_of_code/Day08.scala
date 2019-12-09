package com.cberes.advent_of_code

object Day08 {
  def readImage(encoded : String, width : Int, height : Int) : Seq[Seq[Int]] = {
    val imageSize = width * height
    (0 until encoded.length by imageSize)
      .map(i => encoded.substring(i, i + imageSize))
      .map(s => s.toCharArray.map(_.toString.toInt))
  }

  def singleLayer(multiLayer : Seq[Seq[Int]]) : Seq[Int] = {
    multiLayer.head.indices
      .map(i => multiLayer.find(layer => layer(i) != 2).map(_(i))
      .getOrElse(0))
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val image = readImage(lines.next(), args(1).toInt, args(2).toInt)
    val targetLayer = image.minBy(_.count(i => i == 0))
    val ones = targetLayer.count(_ == 1)
    val twos = targetLayer.count(_ == 2)
    ones * twos
  }

  def part2(args : Array[String]) : Unit = doWithLines(args.head) { lines =>
    val image = singleLayer(readImage(lines.next(), args(1).toInt, args(2).toInt))
    val width = args(1).toInt
    for (i <- image.indices) {
      if (i % width == 0) {
        println()
      }
      print(if (image(i) == 0) " " else "#")
    }
  }
}
