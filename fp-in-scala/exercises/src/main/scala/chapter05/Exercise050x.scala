package chapter05

class Exercise0501 {

}

sealed trait Stream[+A]
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def toList[A](): List[A] = {
    this match {
      case Cons(h, t) => h() :: (t().toList)
      case _ => Nil
    }
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case Empty => z
    }

  def forAll(p: A => Boolean): Boolean =
    this match {
      case Cons(h, t) => p(h()) && (t().forAll(p))
      case Empty => true
    }
}