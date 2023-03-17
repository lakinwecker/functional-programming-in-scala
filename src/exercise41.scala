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
  @main
  def main = 
    val s: MyOption[Int] = MyOption.Some(4)
    val n: MyOption[Int] = MyOption.None
    println("map:")
    println(s.map((a) => a*a))
    println(n.map((a) => a*a))
    println("\nflatMap:")
    println(s.flatMap((a) => MyOption.Some(a*a)))
    println(n.flatMap((a) => MyOption.Some(a*a)))
    println("\nflatMap:")
    println(s.getOrElse(999))
    println(n.getOrElse(999))
    println("\norElse:")
    println(s.orElse(MyOption.Some(999)))
    println(n.orElse(MyOption.Some(999)))
    println("\nfilter:")
    println(s.filter((a) => a % 2 == 0))
    println(s.filter((a) => a % 2 == 1))
    println(n.filter((a) => a % 2 == 0))
    println(n.filter((a) => a % 2 == 1))
}


def add(x: Int)(y: Int) = x + y

def add3(y: Int) = add(3)(y)
val add3 = add(3)


