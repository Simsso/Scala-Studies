package chapter04

class Exercise0401 {

}

trait MyOption[+A] {
  def map[B](f: A => B): MyOption[B]

  def flatMap[B](f: A => MyOption[B]): MyOption[B]

  def getOrElse[B >: A](default: => B): B

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B]

  def filter(f: A => Boolean): MyOption[A]
}

case class MySome[+A](get: A) extends MyOption[A] {
  override def map[B](f: A => B): MyOption[B] = MySome(f(this.get))

  override def flatMap[B](f: A => MyOption[B]): MyOption[B] = f(this.get)

  override def getOrElse[B >: A](default: => B): B = this.get

  override def orElse[B >: A](ob: => MyOption[B]): MyOption[B] = this

  override def filter(f: A => Boolean): MyOption[A] = if (f(this.get)) this else MyNone
}

case object MyNone extends MyOption[Nothing] {
  override def map[B](f: Nothing => B): MyOption[B] = this

  override def flatMap[B](f: Nothing => MyOption[B]): MyOption[B] = this

  override def getOrElse[B >: Nothing](default: => B): B = default

  override def orElse[B >: Nothing](ob: => MyOption[B]): MyOption[B] = ob

  override def filter(f: Nothing => Boolean): MyOption[Nothing] = this
}