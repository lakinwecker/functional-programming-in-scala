import scala.collection.immutable.List

object exercise65 {

  trait RNG:
    def nextInt: (Int, RNG)

  type Rand[A] = State[RNG, A]

  opaque type State[S, +A] = S => (A, S)

  object State:
    extension [S, A] (underlying: State[S, A])
      def run(s: S): (A, S) = underlying(s)

      def flatMap[B](f: A => State[S, B]): State[S, B] = rng =>
          val (a, rng2) = underlying(rng)
          f(a)(rng2)

      def map[B](f: A => B): State[S, B] =
        underlying.flatMap[B](a => unit(f(a)))

      def map2[B, C](rb: State[S, B])(f: (A, B) => C): State[S, C] =
        underlying.flatMap(a => rb.flatMap[C]((b: B) => unit[S, C](f(a, b))))

    def sequence[S, A](rs: List[State[S, A]]): State[S, List[A]] =
      rng => rs.foldLeft((List[A](), rng)){
        case ((l, rng), f) => {
          val (a, rng2) = f(rng)
          (l :+ a, rng2)
        }
      }

    def unit[S, A](a: A): State[S, A] = rng => (a, rng)

    def apply[S, A](f: S => (A, S)): State[S, A] = f

  import exercise65.State._
  case class SimpleRNG(seed: Long) extends RNG:
    def nextInt: (Int, RNG) =
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>>16).toInt
      (n, nextRNG)

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    ra.map2(rb)((_,_))


  val int: Rand[Int] = rng => rng.nextInt

  def nonNegativeInt(rng: RNG): (Int, RNG) =
    val (n1, newRNG) = rng.nextInt
    val positive = if n1 < 0 then
      (n1+1).abs
    else
      n1
    (positive, newRNG)

  def nonNegativeEven: Rand[Int] =
    nonNegativeInt.map(i => i - (i % 2))

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

  def ints(count: Int) =
    sequence(List.fill(count)((rng: RNG) => rng.nextInt))


  def nonNegativeLessThan(n: Int): Rand[Int] =
    nonNegativeInt.flatMap(i => {
      val mod = i % n
      if i + (n - 1) - mod >= 0 then
        unit(mod)
      else nonNegativeLessThan(n)
    })

  def nonNegativeIntsLessThan(count: Int, n: Int): Rand[List[Int]] =
    sequence(List.fill(count)(nonNegativeLessThan(n)))

  @main
  def main() =
    val rng1 = SimpleRNG(42)
    println("nonNegativeIntsLessThan(100, 6)")
    val (l1, rng2) = nonNegativeIntsLessThan(100, 6)(rng1)
    println(l1)

    println("intDouble")
    val (l2, rng3) = intDouble(rng2)
    println(l2)

    println("doubleInt")
    val (l3, rng4) = doubleInt(rng3)
    println(l3)


}
