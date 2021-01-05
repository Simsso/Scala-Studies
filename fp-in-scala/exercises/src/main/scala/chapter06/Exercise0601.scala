package src.main.scala.chapter06

trait  RNG {
  def nextInt: (Int, RNG)
}

case class SimpleRNG(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}

object Exercise0601 {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, nextRng) = rng.nextInt
    if (num == Int.MinValue)
      nonNegativeInt(nextRng)
    if (num < 0) (-num, nextRng) else (num, nextRng)
  }

  def double(rng: RNG): (Double, RNG) = {
    /*
    Double values are pretty low, not really uniform between [0,1).
    It's unclear why that is.
     */
    val (posInt, nextRng) = nonNegativeInt(rng)
    val posDouble = posInt.toDouble / Int.MaxValue
    if (posDouble == 1)
      double(nextRng)
    (posDouble, nextRng)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (intVal, rng2) = rng.nextInt
    val (doubleVal, rng3) = double(rng2)
    ((intVal, doubleVal), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((d, i), nextRng) = intDouble(rng)
    ((i, d), nextRng)
  }

  def double3(rng: RNG) : ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count <= 0) {
      (List.empty, rng)
    }
    else {
      val (intVal, nextRng) = rng.nextInt
      val (resultList, lastRng) = ints(count - 1)(nextRng)
      (intVal :: resultList, lastRng)
    }
  }

  def main(args: Array[String]): Unit = {
    val range = 1 to 10
    range.foreach(i => println(nonNegativeInt(SimpleRNG(i))))
    range.foreach(i => println(double(SimpleRNG(i))))

    println(ints(10)(SimpleRNG(1706)))
  }
}

