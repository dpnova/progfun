package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 20) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    def pascalIter(c: Int, r: Int): Int = {
      if (c > r) 0
      else if (r == 0 || r == 1 || r == c || c == 0) 1
      else pascalIter(c, r - 1) + pascalIter(c - 1, r - 1)
    }
    if (c == 0 || r == 0) 1
    else pascalIter(c, r - 1) + pascalIter(c - 1, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(count: Int, chars: List[Char]): Int = {
      if (chars.isEmpty) count
      else if (chars.head == '(')
        balanceIter(count + 1, chars.tail)
      else if (chars.head == ')')
        if (count == 0)
          balanceIter(1000, "".toList)
        else
          balanceIter(count - 1, chars.tail)
      else balanceIter(count, chars.tail)
    }
    balanceIter(0, chars) == 0
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, coins: List[Int]): Int = {
      if(money < 0) 0
      else if(money == 0) 1
      else if(coins.isEmpty) 0
      else if(coins.head <= money) 
        countChangeIter(money, coins.tail) + 
          countChangeIter(money - coins.head, coins)
      else
        countChangeIter(money, coins.tail)
    }
    if(coins.isEmpty || money == 0) 0
    countChangeIter(money, coins.sortBy(x => -x))
  }
}
