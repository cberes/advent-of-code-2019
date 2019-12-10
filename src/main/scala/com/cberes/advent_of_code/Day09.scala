package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.{ParameterGetter, State, binaryOp, quit, toInput}
import com.cberes.advent_of_code.Day05.{compute, condition, jump, output, parameterMode, setValue}

import scala.collection.mutable

object Day09 {
  object NowWithRelativeMode extends ParameterGetter {
    override def apply(state: State, offset: Int, isTarget: Boolean) : Long = parameterMode(state, offset) match {
      case 0 => if (isTarget) state.valueAtOffset(offset) else state(state.valueAtOffset(offset))
      case 1 => state.valueAtOffset(offset)
      case 2 => if (isTarget) state.valueAtOffset(offset) + state.relativeBase else state(state.valueAtOffset(offset) + state.relativeBase)
    }
  }

  def adjustRelativeBase(getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, 9) =>
    val value = getter(state, 1)

    state.copy(
      pointer = state.pointer + 2,
      relativeBase = state.relativeBase + value)
  }

  def computer(inputFunc : () => Long, outputFunc : Long => Unit) : PartialFunction[(State, Int), State] =
    binaryOp(1, _ + _, NowWithRelativeMode) orElse
      binaryOp(2, _ * _, NowWithRelativeMode) orElse
      setValue(inputFunc, NowWithRelativeMode) orElse
      output(outputFunc, NowWithRelativeMode) orElse
      jump(5, _ != 0, NowWithRelativeMode) orElse
      jump(6, _ == 0, NowWithRelativeMode) orElse
      condition(7, _ < _, NowWithRelativeMode) orElse
      condition(8, _ == _, NowWithRelativeMode) orElse
      adjustRelativeBase(NowWithRelativeMode) orElse
      quit

  def part1(args : Array[String]) : List[Long] = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val inputs = new mutable.Queue[Long]
    inputs.enqueueAll(args.tail.map(_.toLong))
    val outputs = new mutable.ListBuffer[Long]
    println(compute(state, computer(inputs.dequeue, outputs.append)).memory)
    outputs.toList
  }
}
