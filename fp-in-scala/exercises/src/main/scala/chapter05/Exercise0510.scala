package chapter05

class Exercise0510 {  // +
  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] = {
      Stream.cons(a + b, go(b, a + b))
    }
    Stream.cons(1, Stream.cons(1, go(1, 1)))
  }
}
