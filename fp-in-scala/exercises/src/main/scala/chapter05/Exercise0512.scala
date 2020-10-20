package src.main.scala.chapter05

import chapter05.Stream

class Exercise0512 {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Stream.empty
    }
  }

  def fibsWithUnfold(): Stream[Int] =
    unfold((0, 1)){case (a, b) => Some((a + b, (b, a + b)))}

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(n)(s => Some(s, s + 1))

  def constantWithUnfold[A](a: A): Stream[A] = unfold(None)(_ => Some(a, None))

  def onesWithUnfold(): Stream[Int] = constantWithUnfold(1)
}
