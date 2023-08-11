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

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng =>
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_,_))

  val int: Rand[Int] = rng => rng.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n1, newRNG) = rng.nextInt
    val positive = if n1 < 0 then
      (n1+1).abs
    else
      n1
    (positive, newRNG)

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - (i % 2))

  def double(rng: RNG): (Double, RNG) =
    val (n, newRNG) = nonNegativeInt(rng)
    (n.toDouble / Int.MaxValue.toDouble, newRNG)

  val intDouble: Rand[(Int, Double)] =
    both(int, double)

  val doubleInt: Rand[(Double, Int)] =
    both(double, int)

  def double3(rng: RNG): ((Double, Double, Double), RNG) =
    val (d1, newRNG1) = double(rng)
    val (d2, newRNG2) = double(newRNG1)
    val (d3, newRNG3) = double(newRNG2)
    ((d1, d2, d3), newRNG3)

  def sequence[A](rs: List[Rand[A]]): Rand[List[A]] =
    rng => rs.foldLeft((List[A](), rng)){
      case ((l, rng), f) => {
        val (a, rng2) = f(rng)
        (l :+ a, rng2)
      }
    }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) =
    sequence[Int](List.fill(count)(rng => rng.nextInt))(rng)

  @main
  def main() =
    val rng1 = SimpleRNG(42)
    val (l1, rng2) = ints(5)(rng1)
    println(l1)
    val (l2, rng3) = ints(5)(rng2)
    println(l2)


}
