import scala.collection.immutable.List

object exercise64 {

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

  def intsNonFunctional(count: Int)(rng: RNG): (List[Int], RNG) =
    var n: List[Int] = List()
    var rngTmp = rng
    (1 to count).foreach((_) => {
      val (i, newRng) = rngTmp.nextInt
      rngTmp = rngTmp
      n = n :+ i
    })
    (n, rngTmp)

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    (1 to count).foldLeft[(List[Int], RNG)]((List(), rng)){
      case ((l, rng), _) => {
        val (i, newRng) = rng.nextInt
        (l :+ i, newRng)
      }
    }

  @main
  def main() =
    val rng1 = SimpleRNG(42)
    println("intsNonFunctional(3)")
    val (l1, _) = intsNonFunctional(3)(rng1)
    println(l1)
    val (l2, rng2) = ints(3)(rng1)
    println(l2)

    println("intsNonFunctional(4)")
    val (l3, _) = intsNonFunctional(4)(rng2)
    println(l3)
    val (l4, rng3) = ints(4)(rng2)
    println(l4)

}
