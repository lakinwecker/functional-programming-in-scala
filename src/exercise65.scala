import scala.collection.immutable.List

object exercise65 {


  trait RNG:
    def nextInt: (Int, RNG)

  type Rand[+A] = RNG => (A, RNG)

  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>>16).toInt
      (n, nextRNG)

  def unit[A](a: A): Rand[A] = rng => (a, rng)
  val int: Rand[Int] = rng => rng.nextInt

  def nonNegativeInt: Rand[Int] =
    rng =>
      val (n1, newRNG) = rng.nextInt
      val positive = if n1 < 0 then
        (n1+1).abs
      else
        n1
      (positive, newRNG)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)


  def double: Rand[Double] =
    map(nonNegativeInt)(_.toDouble / Int.MaxValue.toDouble)



  @main
  def main() =
    val rng1 = SimpleRNG(42)
    println("double()")
    val (l1, rng2) = double(rng1)
    println(l1)

    println("double()")
    val (l2, rng3) = double(rng2)
    println(l2)

}
