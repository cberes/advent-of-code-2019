package com.cberes.advent_of_code

import java.util.concurrent.{Executors, LinkedBlockingQueue}

import com.cberes.advent_of_code.Day02.{State, toInput}
import com.cberes.advent_of_code.Day05.{compute, computer}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.{Await, ExecutionContext, ExecutionContextExecutor, Future}
import scala.concurrent.duration._

object Day07 {
  @tailrec
  def solve(state : State, input : List[Int], previousOutput : Long = 0) : Long = input match {
    case phaseSetting :: rest =>
      val output = computeSeries(state, List(phaseSetting, previousOutput))
      solve(state, rest, output.head)
    case Nil => previousOutput
  }

  def maxOutput(state : State, n : Int) : Long = allInputCombinations(n).map(it => solve(state, it)).max

  def solveFeedback(state : State, input : List[Int]) : Long = {
    val inputQueues = input.map(_ => new LinkedBlockingQueue[Long]())
    input.indices.foreach(i => inputQueues(i).offer(input(i)))
    inputQueues.head.offer(0)

    val computers = input.indices.map(i => computer(inputQueues(i).take, inputQueues((i + 1) % input.size).offer(_)))

    val executor = Executors.newFixedThreadPool(input.size)
    implicit val context : ExecutionContextExecutor = ExecutionContext.fromExecutor(executor)

    val futs = computers.map(c => Future { compute(state, c) })

    Await.result(futs.last, 30.seconds)

    executor.shutdownNow()

    inputQueues.head.peek()
  }

  def maxOutputFeedback(state : State, n : Int, minSetting : Int) : Long =
    allInputCombinations(n, minSetting).map(it => solveFeedback(state, it)).max

  def computeSeries(state : State, inputs : Seq[Long]) : Seq[Long] = {
    val inputQueue = new mutable.Queue[Long]
    inputQueue.enqueueAll(inputs)
    val outputs = new mutable.ListBuffer[Long]
    compute(state, computer(inputQueue.dequeue, outputs.append))
    outputs.toSeq
  }

  def allInputCombinations(n : Int, minSetting : Int = 0) : Seq[List[Int]] = {
    (0 until Math.pow(10, n).toInt)
      .map(_.toString.reverse.padTo(n, '0').reverse)
      .map(s => (0 until s.length).map(i => s.substring(i, i + 1).toInt).toList)
      .filter(_.forall(it => it >= minSetting && it < n + minSetting))
      .filter(_.toSet.size == n)
  }

  def part1(args : Array[String]) : Long = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    maxOutput(state, 5)
  }

  def part2(args : Array[String]) : Long = doWithLines(args.head) { lines =>
    val state = lines.next().toState
    maxOutputFeedback(state, 5, 5)
  }
}
