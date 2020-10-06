package chapter05

class Exercise0508 {
  def constant[A](a: A): Stream[A] = {
    val x = Stream.cons(a, x)
    x
  }
}
