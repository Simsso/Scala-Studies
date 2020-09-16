package chapter04

object Exercise0407 {
  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    /*
    The solution looks fine but is not elegant.
    Also the runtime is probably quadratic because of the 'append to list' part.
     */
    es.foldLeft(Right(List()): Either[E, List[A]])((aggr: Either[E, List[A]], nextVal: Either[E, A]) => {
      aggr match {
        case Left(e) => Left(e)
        case Right(l) => nextVal match {
          case Left(e) => Left(e)
          case Right(x) => Right(l :+ x)
        }
      }
    })
  }

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    /*
    Same problems as above, the nesting can probably be removed with map2. (?)
     */
    as.foldLeft(Right(List()): Either[E, List[B]])((aggr: Either[E, List[B]], nextVal: Either[E, A]) => {
      aggr match {
        case Left(e) => Left(e)
        case Right(l) => nextVal match {
          case Left(e) => Left(e)
          case Right(x) => {
            val fx = f(x)
            fx match {
              case Left(x) => Left(e)
              case Right(x) => Right(l := x)
            }
          }
        }
      }
    })
  }
}
