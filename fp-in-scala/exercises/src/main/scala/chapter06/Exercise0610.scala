package src.main.scala.chapter06

class Exercise0610 {

}


case class State[S, +A](run: S => (A, S)) {
  // State is rather a state transition + read/generation than a state

  def map[B](f: A => B): State[S, B] =
    State { sOld =>
      val (a, sNew) = this.run(sOld)
      (f(a), sNew)
    }

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State { s1 =>
      val (a, s2) = this.run(s1)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    }

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State{s1 =>
      val (a, s2) = this.run(s1)
      g(a).run(s2)
    }
}

object State {

  def unit[A](a: A): State[S, A] = State((s: S) => (a, s))

  def sequence[A](fs: List[State[S, A]]): State[S, List[A]] =
    State(sOld => {
      fs.foldRight((List.empty: List[A], sOld))((ra, acc) => {
        val (lst, sOld) = acc
        val (a, sNew) = ra.run(sOld)
        (a :: lst, sNew)
      })
    })

}