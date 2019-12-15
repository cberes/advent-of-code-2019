package com.cberes

import scala.io.Source

package object advent_of_code {
  def doWithLines[T](name : String)(func : Iterator[String] => T) : T = {
    val input = Source.fromFile(name)
    val result = func(input.getLines())
    input.close()
    result
  }

  def resourceIterator(res : String) : Iterator[String] =
    Source.fromResource(res).getLines()
}
