package com.cberes.advent_of_code

import scala.collection.mutable

object Day10 {
  case class Point(x : Int, y : Int) extends Ordered[Point] {
    override def compare(that : Point): Int = {
      val yDiff = this.y - that.y
      if (yDiff == 0) this.x - that.x else yDiff
    }
  }

  case class Line(a : Point, b : Point) {
    private val tolerance = 1E-8

    val slope : Double = {
      if (a.x == b.x && a.y < b.y) Double.MaxValue
      else if (a.x == b.x) Double.MinValue
      else (b.y - a.y).toDouble / (b.x - a.x).toDouble
    }

    val yIntercept : Double = a.y - slope * a.x

    def intersects(c : Point) : Boolean = areReallyClose(y(c.x), c.y)

    private def areReallyClose(d : Double, i : Int) : Boolean = d <= i.toDouble + tolerance && d >= i.toDouble - tolerance

    def x(y : Double) : Double = (y - yIntercept) / slope

    def y(x : Double) : Double = slope * x + yIntercept

    private def xRange = a.x.min(b.x) to a.x.max(b.x)

    private def yRange = a.y.min(b.y) to a.y.max(b.y)

    def intPointsBetween : Seq[Point] = {
      if (a.y == b.y) {
        xRange.map(Point(_, a.y)).filterNot(c => a == c || b == c)
      } else if (a.x == b.x) {
        yRange.map(Point(a.x, _)).filterNot(c => a == c || b == c)
      } else {
        xRange.flatMap(x => yRange.map(y => Point(x, y)))
          .filter(intersects)
          .filterNot(c => a == c || b == c)
      }
    }

    /*
     * x = x0, y < y0: angle =   0
     * x = x0, y > y0: angle = 180
     * x > x0, y = y0: angle =  90
     * x < x0, y = y0: angle = 270
     *
     * top    right: x > x0, y < y0    ◢   90 - θ = invtan(y / x)
     * bottom right: x > x0, y > y0    ◥   90 + θ = invtan(y / x)
     * bottom left:  x < x0, y > y0  ◤    270 - θ = invtan(y / x)
     * top    left:  x < x0, y < y0  ◣    270 + θ = invtan(y / x)
     *
     * sohcahtoa
     *
     */
    def angle : Double = {
      if (b.x == a.x && b.y > a.y) 180
      else if (b.x > a.x && b.y == a.y) 90
      else if (b.x < a.x && b.y == a.y) 270
      else if (b.x > a.x && b.y < a.y) 90 - math.atan(slope.abs).toDegrees
      else if (b.x > a.x && b.y > a.y) 90 + math.atan(slope.abs).toDegrees
      else if (b.x < a.x && b.y > a.y) 270 - math.atan(slope.abs).toDegrees
      else if (b.x < a.x && b.y < a.y) 270 + math.atan(slope.abs).toDegrees
      else 0
    }
  }

  type Stars = Map[Point, mutable.Set[Point]]

  class Space(val stars : Stars, val width : Int, val height : Int) {
    findVisibleStars()

    private def findVisibleStars() : Unit = {
      for (a <- this.stars.keys;
           b <- this.stars.keys if b > a) {
        val line = Line(a, b)

        if (!line.intPointsBetween.exists(this.stars.contains)) {
          this.stars(a).add(b)
          this.stars(b).add(a)
        }
      }
    }

    def bestVisibility : (Point, Int) = {
      val entry = stars.maxBy(_._2.size)

      (entry._1, entry._2.size)
    }

    def clockwiseFromBest : Seq[Point] = clockwiseFrom(bestVisibility._1)

    def clockwiseFrom(p : Point) : Seq[Point] = {
      val visible = stars(p)
      visible.toList.sortBy(other => {
        val line = Line(p, other)
        line.angle
      })
    }
  }

  def readSpace(lines : Iterator[String]) : Space = {
    val charsByCoord = lines.zipWithIndex.flatMap(lineWithIndex => {
      val (line, y) = lineWithIndex

      line.toCharArray.zipWithIndex.map(charWithIndex => {
        val (c, x) = charWithIndex

        Point(x, y) -> c
      })
    }).toList

    val stars = charsByCoord
      .filter(_._2 == '#')
      .map(_._1 -> mutable.Set.empty[Point])
      .toMap

    new Space(stars, charsByCoord.map(_._1.x).max + 1, charsByCoord.map(_._1.y).max + 1)
  }

  def part1(args : Array[String]) : (Point, Int) = doWithLines(args.head) { lines =>
    val space = readSpace(lines)
    space.bestVisibility
  }

  def part2(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val space = readSpace(lines)
    val sorted = space.clockwiseFromBest
    val Point(x, y) = sorted(199)
    x * 100 + y
  }
}
