package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day10.{Line, Point, readSpace}
import org.scalatest._

class Day10Spec extends FlatSpec with Matchers {
  "Day 10" can "find points on a line" in {
    val line = Line(Point(0,0), Point(6,9))
    println(line.intPointsBetween)
  }

  "Day 10" can "solve example 1" in {
    val space = readSpace(".#..#\n.....\n#####\n....#\n...##".split('\n').iterator)
    val best = space.bestVisibility
    best._1 shouldEqual Point(3,4)
    best._2 shouldEqual 8
  }

  "Day 10" can "solve example 2" in {
    val space = readSpace("......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####".split('\n').iterator)
    val best = space.bestVisibility
    best._1 shouldEqual Point(5,8)
    best._2 shouldEqual 33
  }

  "Day 10" can "solve example 3" in {
    val space = readSpace("#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.".split('\n').iterator)
    val best = space.bestVisibility
    best._1 shouldEqual Point(1,2)
    best._2 shouldEqual 35
  }

  "Day 10" can "solve example 4" in {
    val space = readSpace(".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..".split('\n').iterator)
    val best = space.bestVisibility
    best._1 shouldEqual Point(6,3)
    best._2 shouldEqual 41
  }

  "Day 10" can "solve example 5" in {
    val space = readSpace(".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##".split('\n').iterator)
    val best = space.bestVisibility
    best._1 shouldEqual Point(11,13)
    best._2 shouldEqual 210
  }

  "Day 10" can "solve part 2" in {
    val space = readSpace(".#....#####...#..\n##...##.#####..##\n##...#...#.#####.\n..#.....#...###..\n..#.#.....#....##".split('\n').iterator)
    val ordered = space.clockwiseFrom(Point(8,3))
    ordered.foreach(println)
  }
}
