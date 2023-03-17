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
    mean(xs).flatMap(m => mean(xs.map((x) => math.pow(x - m, 2))))


  @main
  def main = 
    val values: List[Double] = List(1.0, 2.5, 4.5, 2.1, 4.1, 5.6, 8.9)
    println(s"mean: ${mean(values)}")
    println(s"variance: ${variance(values)}")
}
