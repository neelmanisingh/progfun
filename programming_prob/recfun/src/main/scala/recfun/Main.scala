package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c < 0 || c > r) 0
    else if (c == 0 && r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceHelper(chars: List[Char], accum: List[Char]): Boolean = {
      if(chars.isEmpty && accum.isEmpty) true
      else if(chars.isEmpty && !accum.isEmpty) false
      else {
        val head = chars.head
        val accumLast = if (accum.isEmpty) "" else accum.last
        if(accumLast.equals('(') && head.equals(')')) {
          balanceHelper(chars.tail, accum.tail)
        } else if(head.equals(')') || head.equals('(')) {
          balanceHelper(chars.tail, accum :+ head)
        } else {
          balanceHelper(chars.tail, accum)
        }
      }

    }
    balanceHelper(chars, List[Char]())
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {

    def countChangeHelper(money: Int, coins: List[Int], lastCoin: Int): Int = {

      if(money < 0) 0
      else if (money == 0) 1
      else {
        coins.zipWithIndex.map(x => {
          if(lastCoin >= x._1) countChangeHelper(money - x._1, coins, x._1)
          else 0
        }).sum
      }
    }

    countChangeHelper(money, coins.sorted, Int.MaxValue)
  }
}
