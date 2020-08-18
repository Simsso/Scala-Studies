package chapter02

object Exercise0202 {
  def all(xs: Iterable[Boolean]): Boolean = {
    def and(x1: Boolean, x2: Boolean): Boolean = x1 && x2
    xs.reduce(and)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    if (as.length < 2) true
    else {
      def localCheck(i: Int): Boolean = ordered(as(i - 1), as(i))
      this.all((1 until as.length).map(localCheck))
    }
  }

  def main(args: Array[String]): Unit = {
    def intLtOrEq(x1: Int, x2: Int): Boolean = x1 <= x2

    val xs = Array(1, 2, 3, 4)
    println(isSorted(xs, intLtOrEq))
    val xsFalse = Array(1, 2, 5, 3, 4)
    println(isSorted(xsFalse, intLtOrEq))
  }
}
