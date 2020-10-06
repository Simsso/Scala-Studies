package chapter05

import java.util.Optional

class Exercise0511 {
  def unfold[A, S](z: S)(f: S => Optional[(A, S)]): Stream[A] = {
    f(z) match {
      case Right(a, s) => Stream.cons(a, unfold(s))
      case Nil => Nil
    }
  }
}
