package chapter05

class Exercise0509 {
  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }
}
