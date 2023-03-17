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

  //def map2_b[EE >: E, B, C](that: MyEither[EE, B])(f: (A, B) => C): MyEither[EE, C] =
    //for
      //a <- this
      //b <- that
    //yield f(a, b)

object MyEither:
  def catchNonFatal[A](a: => A): MyEither[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)


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
    100.0 + a * 1.5 * scala.math.pow(numberOfSpeedingTickets,2)

  @main
  def main =
    val e1: MyEither[String, Int] = MyEither.Left("lakin")
    val e2: MyEither[String, Int] = MyEither.Right(5)
    val e3: MyEither[String, Int] = MyEither.Right(10)
    println("-"*80)
    println(e1.flatMap(canError))
    println(e2.flatMap(canError))
    println(e3.flatMap(canError))

    println("-"*80)
    println(e1.map(wontError))
    println(e2.map(wontError))
    println(e3.map(wontError))

    println("-"*80)
    println(e1.orElse(e2))
    println(e2.orElse(e1))
    println(e3.orElse(e1))

    println("-"*80)
    println(e1.map2(e2)(add))
    println(e2.map2(e3)(add))
    println(e3.map2(e1)(add))

    println("-"*80)
    println(parseInsuranceRateQuote("24", "0"))

}
