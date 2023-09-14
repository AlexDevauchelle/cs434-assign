package recfun
import common._

import scala.annotation.tailrec

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
    if (r < 0 || c > r || c<0) 0
    else if (r == 0) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def recBalance(chars: List[Char], x: Int): Boolean = {
      if (x < 0) return false
      if (chars.isEmpty) return x==0
      if (chars.head == '(') recBalance(chars.tail,x + 1)
      else if (chars.head == ')') recBalance(chars.tail,x - 1)
      else recBalance(chars.tail,x)
    }
    recBalance(chars,0)
  }


  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
