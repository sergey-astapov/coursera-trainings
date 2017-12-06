package reductions

import java.lang.Math.min

import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx >= until) (arg1, arg2)
      else if (arg1 < 0) (arg1, arg2)
      else chars(idx) match {
        case '(' => traverse(idx + 1, until, arg1 + 1, arg2 + 1)
        case ')' => traverse(idx + 1, until, min(arg1, arg2 - 1), arg2 - 1)
        case _ => traverse(idx + 1, until, arg1, arg2)
      }
    }

    traverse(0, chars.length, 0, 0) == (0, 0)
//    parBalance(chars, 1)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {

    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int): (Int, Int) = {
      if (idx >= until) (arg1, arg2)
      else if (arg1 < 0) (arg1, arg2)
      else chars(idx) match {
        case '(' => traverse(idx + 1, until, arg1 + 1, arg2 + 1)
        case ')' => traverse(idx + 1, until, min(arg1, arg2 - 1), arg2 - 1)
        case _ => traverse(idx + 1, until, arg1, arg2)
      }
    }

    def reduce(from: Int, until: Int): (Int, Int) = {
      val diff = until - from
      if (diff <= threshold) traverse(from, until, 0, 0)
      else {
        val (a, b) = parallel(reduce(from, from + diff / 2), reduce(from + diff / 2, until))
        (min(a._1, a._2 + b._1), a._2 + b._2)
      }
    }

    if (chars.isEmpty) true
    else {
      reduce(0, chars.length) == (0, 0)
    }
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
