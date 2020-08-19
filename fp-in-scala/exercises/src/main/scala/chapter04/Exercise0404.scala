package chapter04

object Exercise0404 {

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    def aggr(nextElem: Option[A], accumulator: Option[List[A]]): Option[List[A]] = {
      if (nextElem.isEmpty || accumulator.isEmpty) None
      else Some(nextElem.get :: accumulator.get)
    }

    val z: Option[List[A]] = Some(List())
    a.foldRight(z)(aggr)
  }

  /*
  def sequence1[A](a: List[Option[A]]): Option[List[A]] = {
    def aggr(xs: Option[List[A]], x: Option[A]): Option[List[A]] = {
      if (xs == None) None
      else if (x == None) None
      else {
        def prepend(l: List[A]) = x :: l

        xs.flatMap(prepend)
      }
    }

    a.foldLeft(Some(List()))(aggr)
  }
  */
}
