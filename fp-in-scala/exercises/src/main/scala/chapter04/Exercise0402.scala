package chapter04

object Exercise0402 {
  def variance(xs: Seq[Double]): Option[Double] = {
    val m: Option[Double] = if (xs.isEmpty) None else Some(xs.sum / xs.length)
    val perElementVariance: Seq[Option[Double]] = xs.map(x => m.flatMap(m => math.pow(x - m, 2)))
    val meanVariance = ???
  }
}
