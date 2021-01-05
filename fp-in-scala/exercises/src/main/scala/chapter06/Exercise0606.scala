package src.main.scala.chapter06

case object Exercise0606 {
  type Rand[+A] = RNG => (A, RNG)

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = ra(rng)
    val (b, rng3) = rb(rng2)
    (f(a, b), rng3)
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    fs.foldRight((List.empty: List[A], rng))((ra, acc) => {
      val (lst, rngPrev) = acc
      val (a, rng) = ra(rngPrev)
      (a :: lst, rng)
    })
  }
}
