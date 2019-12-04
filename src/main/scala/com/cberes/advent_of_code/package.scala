package com.cberes

import scala.io.Source

package object advent_of_code {
  val inputDir : String = sys.props("user.dir") + "/input/"

  def doWithLines[T](name : String)(func : Iterator[String] => T) : T = {
    val input = Source.fromFile(name)
    val result = func(input.getLines())
    input.close()
    result
  }
}
