object exercise61 {

  trait RNG:
    def nextInt: (Int, RNG)

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>>16).toInt
      (n, nextRNG)

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n1, newRNG) = rng.nextInt
    val positive = if n1 < 0 then
      (n1+1).abs
    else
      n1
    (positive, newRNG)

  @main
  def main() =
    val rng1 = SimpleRNG(42)
    val (n1, rng2) = rng1.nextInt
    println(n1)
    val (n2, rng3) = rng2.nextInt
    println(n2)
    val (n3, rng4) = rng3.nextInt
    println(n3)

    println("Non Negative")
    val (n4, rng5) = nonNegativeInt(rng4)
    println(n4)
    val (n5, rng6) = nonNegativeInt(rng5)
    println(n5)
    // we know rng2 produces a negative value.
    val (n6, rng7) = nonNegativeInt(rng2)
    println(n6)

}
