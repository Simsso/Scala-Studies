package chapter04

object Exercise0403 {
  /*
  What is the relationship of b.map(btoC) and lift?
  Found this one comparably tricky, even though the solution ended up being simple.
 */
  def curry[A, B, C](f: (A, B) => C): A => B => C = a => b => f(a, b)

  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {

    val curriedF = curry(f)
    val partialF: Option[B => C] = a.map(curriedF)

    def applyBtoC(btoC: B => C): Option[C] = b.map(btoC)

    partialF.flatMap(applyBtoC)
  }

  def map22[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(a => b.map(b => f(a, b)))
  }

  def map23[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    for {
      aVal <- a
      bVal <- b
    } yield f(aVal, bVal)
  }
}
