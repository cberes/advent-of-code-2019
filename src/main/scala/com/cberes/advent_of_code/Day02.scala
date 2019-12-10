package com.cberes.advent_of_code

import scala.annotation.tailrec

object Day02 {
  trait ParameterGetter {
    def apply(state : State, offset : Int, isTarget : Boolean = false) : Long
  }

  object AlwaysPositionMode extends ParameterGetter {
    override def apply(state: State, offset: Int, isTarget: Boolean) : Long =
      if (isTarget) state.valueAtOffset(offset) else state(state.valueAtOffset(offset))
  }

  case class State(memory : Map[Long, Long],
                   pointer : Long = 0,
                   done : Boolean = false,
                   relativeBase : Long = 0) {
    def apply(address : Long) : Long = memory.getOrElse(address, 0)

    def valueAtOffset(offset : Long) : Long = memory.getOrElse(pointer + offset, 0)

    def currentValue : Long = valueAtOffset(0)
  }

  class Input(input : String) {
    def toState : State = State(input.split(',').map(_.toLong).zipWithIndex.map(it => (it._2.toLong, it._1)).toMap)
  }

  implicit def toInput(input : String) : Input = new Input(input)

  implicit def toState(input : String) : State = input.toState

  val quit : PartialFunction[(State, Int), State] = { case (state, 99) => state.copy(done = true) }

  def binaryOp(code : Int, op : (Long, Long) => Long, getter : ParameterGetter) : PartialFunction[(State, Int), State] = { case (state, `code`) =>
    val left = getter(state, 1)
    val right = getter(state, 2)
    val value = op(left, right)

    val positionOutput = getter(state, 3, isTarget = true)

    state.copy(
      memory = state.memory + (positionOutput -> value),
      pointer = state.pointer + 4)
  }

  val computer : PartialFunction[(State, Int), State] =
    binaryOp(1, _ + _, AlwaysPositionMode) orElse
      binaryOp(2, _ * _, AlwaysPositionMode) orElse
      quit

  @tailrec
  def compute(state : State) : State = {
    val nextState = computer(state, state.currentValue.toInt)
    if (nextState.done) nextState else compute(nextState)
  }

  def part1(args : Array[String]) : Long = doWithLines(args.head) { lines =>
    val inputState = lines.next().toState
    val modifiedState = State(inputState.memory ++ Map(1L -> args(1).toLong, 2L -> args(2).toLong))
    compute(modifiedState)(0)
  }

  def part2(args : Array[String]) : Any = {
    for {noun <- 0 to 99; verb <- 0 to 99; if part1(Array(args.head, noun.toString, verb.toString)) == args.last.toLong} yield 100 * noun + verb
  }
}
