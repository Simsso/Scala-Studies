package chapter02

object Exercise0203 {
  def curry[A,B,C](f: (A, B) => C): A => B => C = {
    a => b => f(a, b)
  }
}
