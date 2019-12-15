package com.cberes.advent_of_code

object Day14 {
  case class ComponentQuantity(name : String, quantity : Int) {
    def *(multiplier : Int) : ComponentQuantity = copy(quantity = quantity * multiplier)

    override def toString: String = quantity + " " + name
  }

  case class Formula(input : List[ComponentQuantity], output : ComponentQuantity) {
    override def toString: String = input.mkString(", ") + " => " + output
  }

  class Nanofactory(allFormulas : Iterator[Formula]) {
    private val formulas : Map[String, List[Formula]] = {
      allFormulas.foldLeft(Map.empty[String, List[Formula]])((acc, formula) => {
        val key = formula.output.name
        val otherFormulas = acc.getOrElse(key, Nil)
        acc + (key -> (formula :: otherFormulas))
      })
    }

    private var excess = Map[String, Int]()

    def apply(resource : String) : List[Formula] = formulas(resource)

    def quantityRequired(resource : String, target : ComponentQuantity) : Int = {
      target.name match {
        case `resource` => target.quantity
        case _ =>
          val inputFormulas = formulas(target.name)
          if (inputFormulas.size > 1) {
            println("uh oh, we have a choice for " + target)
          }

          val excessAvailable = excess.getOrElse(target.name, 0)
          var requiredQuantity = target.quantity

          // some of the required quantity may be available in excess
          if (excessAvailable >= requiredQuantity) {
            val excessRemaining = excessAvailable - requiredQuantity
            excess += target.name -> excessRemaining
            return 0
          } else if (excessAvailable > 0) {
            requiredQuantity -= excessAvailable
            excess -= target.name
          }

          // determine how many multiple of this formula are needed
          val outputQuantity = inputFormulas.head.output.quantity
          val multiplier = math.ceil(requiredQuantity.toDouble / outputQuantity).toInt

          // if we're producing excess, record it
          if (multiplier * outputQuantity > requiredQuantity) {
            val additionalExcess = multiplier * outputQuantity - requiredQuantity
            excess += target.name -> additionalExcess
          }

          // find how much we need of each of the input resources
          val inputs = inputFormulas.head.input
          inputs.map(input => quantityRequired(resource, input * multiplier)).sum
      }
    }

    def totalProduced(resource : String, target : ComponentQuantity, available : Long) : Long = {
      var totalOreUsed = 0L
      var produced = 0L

      while (totalOreUsed < available) {
        val oreUsed = quantityRequired(resource, target)
        totalOreUsed += oreUsed

        if (totalOreUsed <= available) {
          produced = produced + target.quantity + excess.getOrElse(target.name, 0)
          excess -= target.name
        }
      }

      produced
    }
  }

  def readInput(lines : Iterator[String]) : Iterator[Formula] =
    lines.map(_.split("\\s*=>\\s*", 2)).map(toFormula)

  def toFormula(parts : Array[String]) : Formula = {
    val input = parts(0).split("\\s*,\\s*").map(toComponent)
    val output = toComponent(parts(1))
    Formula(input.toList, output)
  }

  def toComponent(s : String) : ComponentQuantity = {
    val parts = s.split("\\s+", 2)
    ComponentQuantity(parts(1), parts(0).toInt)
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val formulas = readInput(lines)
    val factory = new Nanofactory(formulas)
    println(factory("FUEL"))
    factory.quantityRequired("ORE", ComponentQuantity("FUEL", 1))
  }

  def part2(args : Array[String]) : Long = doWithLines(args.head) { lines =>
    val formulas = readInput(lines)
    val factory = new Nanofactory(formulas)
    factory.totalProduced("ORE", ComponentQuantity("FUEL", 1), 1000000000000L)
  }
}
