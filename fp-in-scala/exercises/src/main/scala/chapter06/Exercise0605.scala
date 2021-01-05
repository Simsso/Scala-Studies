package src.main.scala.chapter06

object Exercise0605 {
  type Rand[+A] = RNG => (A, RNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt: Rand[Int] = rng => {
    val (num, nextRng) = rng.nextInt
    if (num == Int.MinValue)
      nonNegativeInt(nextRng)
    if (num < 0) (-num, nextRng) else (num, nextRng)
  }

  def double: Rand[Double] = {
    val d = map(nonNegativeInt)(i => i.toDouble / Int.MaxValue)
    map(d)(i => if (i == 1) 0.0 else i)
  }
}
