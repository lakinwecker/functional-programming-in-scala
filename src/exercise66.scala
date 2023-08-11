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
  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng =>
      val (a, rng2) = s(rng)
      (f(a), rng2)

  val int: Rand[Int] = rng => rng.nextInt

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

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_,_))

  val intDouble: Rand[(Int, Double)] =
    both(int, double)

  val doubleInt: Rand[(Double, Int)] =
    both(double, int)


  @main
  def main() =
    val rng1 = SimpleRNG(42)
    println("intDouble")
    val (l1, rng2) = intDouble(rng1)
    println(l1)

    println("doubleInt")
    val (l2, rng3) = doubleInt(rng2)
    println(l2)


}
