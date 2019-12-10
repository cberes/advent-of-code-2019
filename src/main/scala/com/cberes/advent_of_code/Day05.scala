package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.{ParameterGetter, State, binaryOp, quit, toInput}

import scala.annotation.tailrec
import scala.collection.mutable

object Day05 {
  object PositionAndImmediateMode extends ParameterGetter {
    override def apply(state: State, offset: Int, isTarget: Boolean) : Long = parameterMode(state, offset) match {
      case 0 => if (isTarget) state.valueAtOffset(offset) else state(state.valueAtOffset(offset))
      case 1 => state.valueAtOffset(offset)
    }
  }

  def parameterMode(state : State, oneBasedIndex : Int) : Int = {
    val rawValue = state.currentValue.toString
    val instruction = rawValue.reverse.padTo(2 + oneBasedIndex, '0').take(2 + oneBasedIndex).reverse
    instruction.charAt(0) - '0'
  }

  def computer(inputFunc : () => Long, outputFunc : Long => Unit) : PartialFunction[(State, Int), State] =
    binaryOp(1, _ + _, PositionAndImmediateMode) orElse
      binaryOp(2, _ * _, PositionAndImmediateMode) orElse
      setValue(inputFunc, PositionAndImmediateMode) orElse
      output(outputFunc, PositionAndImmediateMode) orElse
      jump(5, _ != 0, PositionAndImmediateMode) orElse
      jump(6, _ == 0, PositionAndImmediateMode) orElse
      condition(7, _ < _, PositionAndImmediateMode) orElse
      condition(8, _ == _, PositionAndImmediateMode) orElse
      quit

  def setValue(f : () => Long, getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, 3) =>
    val position = getter(state, 1, isTarget = true)

    state.copy(
      memory = state.memory + (position -> f()),
      pointer = state.pointer + 2)
  }

  def output(f : Long => Unit, getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, 4) =>
    val value = getter(state, 1)

    f(value)

    state.copy(pointer = state.pointer + 2)
  }

  def jump(code : Int, test : Long => Boolean, getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, `code`) =>
    val value = getter(state, 1)

    if (test(value)) {
      state.copy(pointer = getter(state, 2))
    } else {
      state.copy(pointer = state.pointer + 3)
    }
  }

  def condition(code : Int, test : (Long, Long) => Boolean, getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, `code`) =>
    val left = getter(state, 1)
    val right = getter(state, 2)
    val position = getter(state, 3, isTarget = true)
    val output = if (test(left, right)) 1 else 0

    state.copy(
      memory = state.memory + (position -> output),
      pointer = state.pointer + 4)
  }

  @tailrec
  def compute(state : State, computer : PartialFunction[(State, Int), State]) : State = {
    val nextState = computer(state, opcode(state))
    if (nextState.done) nextState else compute(nextState, computer)
  }

  def opcode(state : State) : Int = {
    if (state.currentValue.toString.length > 2) {
      state.currentValue.toString.takeRight(2).toInt
    } else {
      state.currentValue.toInt
    }
  }

  def part1(args : Array[String]) : List[Long] = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val inputs = new mutable.Queue[Long]
    inputs.enqueueAll(args.tail.map(_.toLong))
    val outputs = new mutable.ListBuffer[Long]
    compute(state, computer(inputs.dequeue, outputs.append))
    outputs.toList
  }
}
