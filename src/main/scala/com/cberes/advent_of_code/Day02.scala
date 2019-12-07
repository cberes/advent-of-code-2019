package com.cberes.advent_of_code

import scala.annotation.tailrec

object Day02 {
  type ParameterGetter = (State, Int) => Int

  case class State(memory : Map[Int, Int], pointer : Int = 0, done : Boolean = false) {
    def apply(address : Int) : Int = memory(address)

    def valueAtOffset(offset : Int) : Int = memory(pointer + offset)

    def currentValue : Int = valueAtOffset(0)
  }

  class Input(input : String) {
    def toState : State = State(input.split(',').map(_.toInt).zipWithIndex.map(_.swap).toMap)
  }

  implicit def toInput(input : String) : Input = new Input(input)

  implicit def toState(input : String) : State = input.toState

  val quit : PartialFunction[(State, Int), State] = { case (state, 99) => state.copy(done = true) }

  val parameterGetter : ParameterGetter = (state, offset) => state(state.valueAtOffset(offset))

  def binaryOp(code : Int, op : (Int, Int) => Int, getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, `code`) =>
    val positionOutput = state.valueAtOffset(3)

    val left = getter(state, 1)
    val right = getter(state, 2)
    val value = op(left, right)

    state.copy(
      memory = state.memory + (positionOutput -> value),
      pointer = state.pointer + 4)
  }

  val computer : PartialFunction[(State, Int), State] =
    binaryOp(1, _ + _, parameterGetter) orElse
      binaryOp(2, _ * _, parameterGetter) orElse
      quit

  @tailrec
  def compute(state : State) : State = {
    val nextState = computer(state, state.currentValue)
    if (nextState.done) nextState else compute(nextState)
  }

  def part1(args : Array[String]) : Int = doWithLines(args.head) { lines =>
    val inputState = lines.next().toState
    val modifiedState = State(inputState.memory  ++ Map(1 -> args(1).toInt, 2 -> args(2).toInt))
    compute(modifiedState)(0)
  }

  def part2(args : Array[String]) : Any = {
    for {noun <- 0 to 99; verb <- 0 to 99; if part1(Array(args.head, noun.toString, verb.toString)) == args.last.toInt} yield 100 * noun + verb
  }
}
