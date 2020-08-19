package chapter04


object Exercise0405 {
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {
      case e: Exception => None
    }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def aggregate(valA: A, optR: Option[List[B]]): Option[List[B]] = {
      for {
        b <- f(valA)
        r <- optR
      } yield b :: r
    }

    a.foldRight(Some(List()): Option[List[B]])(aggregate)
  }


  def main(args: Array[String]): Unit = {
    processList(List("5", "123", "523", "1"))
    processList(List("5", "123", "523", "1", "aside"))
    processList(List("5", "x", "123", "523", "1"))
  }

  private def processList(intStrs: List[String]): Unit = {
    val ints = this.traverse(intStrs)(s => Try(s.toInt))
    println(ints.getOrElse(List()))
  }
}
