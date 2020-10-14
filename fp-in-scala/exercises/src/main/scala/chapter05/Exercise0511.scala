package chapter05

class Exercise0511 {
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s))
      case None => Stream.empty
    }
  }
}
