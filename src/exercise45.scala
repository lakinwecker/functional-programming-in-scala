import scala.annotation.tailrec

enum MyOption[+A]:
  case Some(get: A)
  case None

  def map[B](f: A => B): MyOption[B] =
    this match {
      case Some(v) => Some(f(v))
      case None => None
    }


  def flatMap[B](f: A => MyOption[B]): MyOption[B] =
    this.map(f).getOrElse(None)

  def getOrElse[B >: A](default: => B): B =
    this match {
      case Some(v) => v
      case None => default
    }

  def orElse[B >: A](ob: => MyOption[B]): MyOption[B] =
    map(Some(_)).getOrElse(ob)


  def filter(f: A => Boolean): MyOption[A] =
    flatMap((a) => if (f(a)) MyOption.Some(a) else MyOption.None)


object Example {
  def mean(xs: Seq[Double]): MyOption[Double] =
    if xs.isEmpty then MyOption.None
    else MyOption.Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): MyOption[Double] =
    mean(xs).flatMap((m) => mean(xs.map((x) => math.pow(x - m, 2))))

  def map2[A, B, C](a: MyOption[A], b: MyOption[B])(f: (A, B) => C): MyOption[C] =
    (a, b) match {
      case (MyOption.Some(a), MyOption.Some(b)) => MyOption.Some(f(a, b))
      case _ => MyOption.None
    }

  def sequence[A](as: List[MyOption[A]]): MyOption[List[A]] =
    traverse(as)(o => o)

  // TODO: redo as a fold
  def traverse[A, B](as: List[A])(f: A => MyOption[B]): MyOption[List[B]] =
    as match {
      case Nil       => MyOption.None
      case a :: Nil  => f(a).map(List(_))
      case a :: rest => f(a).flatMap(a => traverse(rest)(f).map(s => a +: s))
    }

  def complete(a: Int): MyOption[Int] = MyOption.Some(a*a)
  def partial(a: Int): MyOption[Int] =
    if (a == 5) MyOption.None
    else MyOption.Some(a*a)

  @main
  def main =
    println(traverse(List(5, 10))(complete))
    println(traverse(List(5, 10))(partial))

    val a = MyOption.Some(5)
    val b = MyOption.Some(10)
    val n = MyOption.None

    println(sequence(List(a, b)))
    println(sequence(List(b, n)))
    println(sequence(List(n, b)))

}
