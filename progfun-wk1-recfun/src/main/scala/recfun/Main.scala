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
    
    def triangleIter(rowIndex: Int, rowValues: List[Int]) : List[Int] =
      // Terminating condition
      if (rowIndex == r) rowValues
      else triangleIter(rowIndex + 1, calculateNextRowValues(rowValues))
      
    def calculateNextRowValues(currentRowValues: List[Int]) = {

      val newRowValues = (for (index <- 1 until currentRowValues.length) 
        yield currentRowValues(index - 1) + currentRowValues(index)).toList
      List(1) ::: newRowValues ::: List(1)
    }
    
    triangleIter(0, List(1))(c)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    
    def balanceIter(chars: List[Char], n: Int): Boolean = 
      if (n < 0) false
      else if (chars.isEmpty) n == 0
      else {
        if (chars.head == '(') balanceIter(chars.tail, n+1)
        else if (chars.head == ')') balanceIter(chars.tail, n-1)
        else balanceIter(chars.tail, n)
      }
    
    balanceIter(chars, 0)
  }
  
  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    
    def countChangeIter(money: Int, coins: List[Int]): Int = {
      if (money == 0) 1
      else if ((money < 0) || (coins.isEmpty)) 0
      else countChangeIter(money, coins.tail) + countChangeIter(money - coins.head, coins)
    }
    
    countChangeIter(money, coins)
  }
  
}
