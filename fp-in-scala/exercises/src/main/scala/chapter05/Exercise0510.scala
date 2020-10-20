package chapter05

class Exercise0510 {  // +
  def fibs(): Stream[Int] = {
    def go(a: Int, b: Int): Stream[Int] =
      Stream.cons(a, go(b, a + b))
    go(0, 1)
  }

  def fibs: Stream[Int] = {
    lazy val s: Stream[(Int, Int)] =
      Stream.cons((0, 1), s.map(a => (a._1 + a._2, a._1)))
    s.map(_._1)
  }
}
