package chapter04

trait Exercise0406 {

}

trait MyEither[+E, +A] {
  def map[B](f: A => B): MyEither[E, B]

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B]

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B]

  def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C]
}

case class MyLeft[+E](value: E) extends MyEither[E, Nothing] {
  override def map[B](f: Nothing => B): MyEither[E, B] = this

  override def flatMap[EE >: E, B](f: Nothing => MyEither[EE, B]): MyEither[EE, B] = MyLeft(value)

  override def orElse[EE >: E, B >: Nothing](b: => MyEither[EE, B]): MyEither[EE, B] = b

  override def map2[EE >: E, B, C](b: MyEither[EE, B])(f: (Nothing, B) => C): MyEither[EE, C] = MyLeft(value)
}

case class MyRight[+A](value: A) extends MyEither[Nothing, A] {
  override def map[B](f: A => B): MyEither[Nothing, B] = MyRight(f(value))

  override def flatMap[EE >: Nothing, B](f: A => MyEither[EE, B]): MyEither[EE, B] = f(value)

  override def orElse[EE >: Nothing, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] = this

  override def map2[EE >: Nothing, B, C](b: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] = for {bVal <- b} yield f(value, bVal)
}

