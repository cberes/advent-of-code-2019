package com.cberes.advent_of_code

import com.cberes.advent_of_code.Day02.{ParameterGetter, State, binaryOp, quit, toInput}

import scala.annotation.tailrec
import scala.collection.mutable

object Day05 {
  val parameterGetter : ParameterGetter = { (state, offset) =>
    val rawValue = state.currentValue.toString
    val instruction = rawValue.reverse.padTo(2 + offset, '0').take(2 + offset).reverse
    instruction.charAt(0) match {
      case '0' => state(state.valueAtOffset(offset))
      case '1' => state.valueAtOffset(offset)
    }
  }

  def computer(inputFunc : () => Int, outputFunc : Int => Unit) : PartialFunction[(State, Int), State] =
    binaryOp(1, _ + _, parameterGetter) orElse
      binaryOp(2, _ * _, parameterGetter) orElse
      setValue(inputFunc) orElse
      output(outputFunc, parameterGetter) orElse
      jump(5, _ != 0, parameterGetter) orElse
      jump(6, _ == 0, parameterGetter) orElse
      condition(7, _ < _, parameterGetter) orElse
      condition(8, _ == _, parameterGetter) orElse
      quit

  def setValue(f : () => Int) : PartialFunction[(State, Int), State] = { case (state, 3) =>
    val position = state.valueAtOffset(1)

    state.copy(
      memory = state.memory + (position -> f()),
      pointer = state.pointer + 2)
  }

  def output(f : Int => Unit, getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, 4) =>
    val value = getter(state, 1)

    f(value)

    state.copy(pointer = state.pointer + 2)
  }

  def jump(code : Int, test : Int => Boolean, getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, `code`) =>
    val value = getter(state, 1)

    if (test(value)) {
      state.copy(pointer = getter(state, 2))
    } else {
      state.copy(pointer = state.pointer + 3)
    }
  }

  def condition(code : Int, test : (Int, Int) => Boolean, getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, `code`) =>
    val left = getter(state, 1)
    val right = getter(state, 2)
    val position = state.valueAtOffset(3)
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
      state.currentValue
    }
  }

  def part1(args : Array[String]) : List[Int] = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    val inputs = new mutable.Queue[Int]
    inputs.enqueueAll(args.tail.map(_.toInt))
    val outputs = new mutable.ListBuffer[Int]
    compute(state, computer(inputs.dequeue, outputs.append))
    outputs.toList
  }
}