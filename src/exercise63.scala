object exercise63 {

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

  def double(rng: RNG): (Double, RNG) =
    val (n, newRNG) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue.toDouble, newRNG)

  def intDouble(rng: RNG): ((Int, Double), RNG) =
    val (i, newRNG) = rng.nextInt
    val (d, newRNG2) = double(newRNG)
    ((i, d), newRNG2)

  def doubleInt(rng: RNG): ((Double, Int), RNG) =
    val ((i, d), newRNG) = intDouble(rng)
    ((d, i), newRNG)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, newRNG1) = double(rng)
    val (d2, newRNG2) = double(newRNG1)
    val (d3, newRNG3) = double(newRNG2)
    ((d1, d2, d3), newRNG3)

  @main
  def main() =
    val rng1 = SimpleRNG(42)
    println("double3")
    val ((d1, d2, d3), rng2) = double3(rng1)
    println(d1)
    println(d2)
    println(d3)

    println("doubleInt")
    val ((d, i), rng3) = doubleInt(rng2)
    println(d)
    println(i)

    println("intDouble")
    val ((i2, d4), rng4) = doubleInt(rng3)
    println(i2)
    println(d4)
}
