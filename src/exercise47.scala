import scala.util.control.NonFatal

enum MyEither[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def map[B](f: A => B): MyEither[E, B] = 
    this.flatMap(a => MyEither.Right(f(a)))

  def flatMap[EE >: E, B](f: A => MyEither[EE, B]): MyEither[EE, B] =
    this match {
      case Left(e) => Left(e)
      case Right(a) => f(a)
    }

  def orElse[EE >: E, B >: A](b: => MyEither[EE, B]): MyEither[EE, B] =
    this match {
      case Right(a) => Right(a)
      case Left(_) => b
    }

  def map2[EE >: E, B, C](that: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    this.flatMap(a => that.map(b => f(a, b)))

  def map2[EE >: E, B, C](that: MyEither[EE, B])(f: (A, B) => C): MyEither[Tuple2[EE, E], C] =
    this.flatMap(a => that.map(b => f(a, b)))

  //def map2_b[EE >: E, B, C](that: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    //for
      //a <- this
      //b <- that
    //yield f(a, b)

object MyEither:
  def catchNonFatal[A](a: => A): MyEither[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  def sequence[E, A](as: List[MyEither[E, A]]): MyEither[E, List[A]] =
    as.foldLeft[MyEither[E, List[A]]](Right(List[A]())){(l, e) => l.map2(e)((l, e) => l :+ e)}

  def traverse[E, A, B](as: List[A])(f: A => MyEither[E, B]): MyEither[E, List[B]] =
    as.foldLeft[MyEither[E, List[B]]](Right(List[B]())){(l, a) => l.map2(f(a))((l, e) => l :+ e)}

  def sequence2[E, A](as: List[MyEither[E, A]]): MyEither[E, List[A]] =
    traverse(as)(identity)


object Example {

  def canError(i: Int): MyEither[String, Int] = 
    if (i == 0) MyEither.Left("Can't divide by zero")
    else MyEither.Right(100/i)

  def wontError(i: Int): Int = i * 100
  def add(x: Int, y: Int): Int = x + y

  def parseInsuranceRateQuote(age: String, numberOfSpeedingTickets: String): MyEither[Throwable, Double] =
    for
      a <- MyEither.catchNonFatal(age.toInt)
      tickets <- MyEither.catchNonFatal(numberOfSpeedingTickets.toInt)
    yield insuranceRateQuote(a, tickets)

  def insuranceRateQuote(a: Int, numberOfSpeedingTickets: Int) =
    100.0 + (100-a) * 1.5 * scala.math.pow(numberOfSpeedingTickets,2)

  @main
  def main =
    val e1: MyEither[String, Int] = MyEither.Left("lakin")
    val e2: MyEither[String, Int] = MyEither.Right(5)
    val e3: MyEither[String, Int] = MyEither.Right(10)
    println("-"*80)
    println(MyEither.sequence(List(e1, e2, e3)))
    println(MyEither.sequence(List(e2, e3)))

    println("-"*80)
    println(MyEither.sequence2(List(e1, e2, e3)))
    println(MyEither.sequence2(List(e2, e3)))

    println("-"*80)
    val e4: Tuple2[String, String] = ("lakin", "1")
    val e5: Tuple2[String, String] = ("42", "1")
    val e6: Tuple2[String, String] = ("28", "10")
    println(MyEither.traverse(List(e4, e5, e6))((t) => parseInsuranceRateQuote(t._1, t._2)))
    println(MyEither.traverse(List(e5, e6))((t) => parseInsuranceRateQuote(t._1, t._2)))



}
