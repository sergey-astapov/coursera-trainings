package reductions

import org.scalameter._
import common._

object ParallelCountChangeRunner {

  @volatile var seqResult = 0

  @volatile var parResult = 0

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 80,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val amount = 250
    val coins = List(1, 2, 5, 10, 20, 50)
    val seqtime = standardConfig measure {
      seqResult = ParallelCountChange.countChange(amount, coins)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential count time: $seqtime ms")

    def measureParallelCountChange(threshold: ParallelCountChange.Threshold): Unit = {
      val fjtime = standardConfig measure {
        parResult = ParallelCountChange.parCountChange(amount, coins, threshold)
      }
      println(s"parallel result = $parResult")
      println(s"parallel count time: $fjtime ms")
      println(s"speedup: ${seqtime / fjtime}")
    }

    measureParallelCountChange(ParallelCountChange.moneyThreshold(amount))
    measureParallelCountChange(ParallelCountChange.totalCoinsThreshold(coins.length))
    measureParallelCountChange(ParallelCountChange.combinedThreshold(amount, coins))
  }
}

object ParallelCountChange {

  /** Returns the number of ways change can be made from the specified list of
   *  coins for the specified amount of money.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    val sortedCoins = coins.sorted.toArray

    def howMany(m: Int, i: Int): Int = (m, i) match {
      case (x, _) if x < 0 => 0
      case (x, _) if x == 0 => 1
      case (_, index) if index < 0 => 0
      case (x, index) if x < sortedCoins(index) => howMany(x, index - 1)
      case (x, index) if x == sortedCoins(index) => 1 + howMany(x, index - 1)
      case (x, index) =>
        val r = x - sortedCoins(index)
        howMany(r, index) + howMany(x, index - 1)
    }

    howMany(money, sortedCoins.length - 1)
  }

//  def countChange(money: Int, coins: List[Int]): Int = parCountChange(money, coins, (_, _) => false)

  type Threshold = (Int, List[Int]) => Boolean

  sealed trait Tree

  final case class Node(ls: Tree, rs: Tree) extends Tree

  final case class Leaf(count: Int) extends Tree

  /** In parallel, counts the number of ways change can be made from the
   *  specified list of coins for the specified amount of money.
   */
  def parCountChange(money: Int, coins: List[Int], threshold: Threshold): Int = {
    if (threshold(money, coins)) countChange(money, coins)
    else {
      val sortedCoins = coins.sorted.toArray

      def howMany(m: Int, i: Int): Tree = (m, i) match {
        case (x, _) if x < 0 => Leaf(0)
        case (x, _) if x == 0 => Leaf(1)
        case (_, index) if index < 0 => Leaf(0)
        case (x, index) if x < sortedCoins(index) => howMany(x, index - 1)
        case (x, index) if x == sortedCoins(index) => Node(Leaf(1), howMany(x, index - 1))
        case (x, index) =>
          val r = x - sortedCoins(index)
          Node(howMany(r, index), howMany(x, index - 1))
      }

      def traverse(tree: Tree): Int = tree match {
        case Node(ls, rs) => traverse(ls) + traverse(rs)
        case Leaf(c) => c
      }

      traverse(howMany(money, sortedCoins.length - 1))
    }
  }

  /** Threshold heuristic based on the starting money. */
  def moneyThreshold(startingMoney: Int): Threshold = (x, _) => x <= 2 * startingMoney / 3

  /** Threshold heuristic based on the total number of initial coins. */
  def totalCoinsThreshold(totalCoins: Int): Threshold =
    (_, coins) => coins.size <= 2 * totalCoins / 3


  /** Threshold heuristic based on the starting money and the initial list of coins. */
  def combinedThreshold(startingMoney: Int, allCoins: List[Int]): Threshold =
    (x, coins) => x * coins.size <= 2 * startingMoney * allCoins.size / 3
}
