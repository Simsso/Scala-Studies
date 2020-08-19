package chapter02

import scala.annotation.tailrec

object Exercise0201 {
  def fibonacci(n: Int): Int = {
    if (n == 0) 0
    else if (n == 1) 1
    else this.fibonacci(n - 1) + this.fibonacci(n - 2)
  }

  def fibonacciTailRecursive(n: Int): Int = {
    @tailrec
    def go(fibpp: Int, fibp: Int, ctr: Int): Int = {
      if (ctr == n) fibpp + fibp
      else go(fibp, fibp + fibpp, ctr + 1)
    }

    if (n == 0) 0
    else if (n == 1) 1
    else go(0, 1, 2)
  }

  def main(args: Array[String]): Unit = {
    val range = 1 to 10
    val fibNumbersA = range.map(this.fibonacciTailRecursive)
    println(fibNumbersA)
    val fibNumbersB = range.map(this.fibonacci)
    println(fibNumbersB)
  }
}
