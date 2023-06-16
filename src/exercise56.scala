import scala.util.control.NonFatal

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])


  def toList: List[A] = this match 
    case Empty => List()
    case Cons(h, rest) => List(h()) ++ rest().toList

  def take(n: Int): LazyList[A] =
    if (n <= 0) LazyList.empty
    else this match
      case Empty => this
      case Cons(h, t) => LazyList.cons(h(), t().take(n-1))

  def drop(n: Int): LazyList[A] = 
    if (n <= 0) this
    else this match
      case Empty => this
      case Cons(h, rest) => rest().drop(n-1)


  def foldRight[B](acc: => B)(f: (A, =>B) => B): B =
    this match
      case Cons(h, t) => f(h(), t().foldRight(acc)(f))
      case _ => acc

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def takeWhile(p: A => Boolean): LazyList[A] =
    foldRight[LazyList[A]](LazyList.Empty)((a, b) => if p(a) then LazyList.cons(a, b) else LazyList.Empty)

  def headOption: Option[A] = foldRight[Option[A]](None)((a, b) => Some(a))

object LazyList:
  def cons[A](hd: => A, tl: => LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))


  def lazyMake[A](a: A): A =
    // println(s"Making: ${a}")
    a

  def isOdd(a: Int): Boolean = a % 2 == 1
  def isEven(a: Int): Boolean = a % 2 == 0
  def isPositive(a: Int): Boolean = a > 0
  def largerThan(limit: Int)(a: Int): Boolean = a > limit
  def lessThan(limit: Int)(a: Int): Boolean = a < limit

  @main
  def main =
    val emptyList = LazyList.Empty
    val lazyList =
      LazyList.cons(
        lazyMake(1),
        LazyList.cons(
          lazyMake(2),
          LazyList.cons(
            lazyMake(3),
            LazyList.cons(
              lazyMake(4),
              LazyList.cons(lazyMake(5), Empty)
            )
          )
        )
    )
    println("forAll(isOdd)")
    println(lazyList.forAll(isOdd))
    println("forAll(isEven)")
    println(lazyList.forAll(isEven))
    println("forAll(isPositive)")
    println(lazyList.forAll(isPositive))
    println("takeWhile(lessThan(3))")
    println(lazyList.takeWhile(lessThan(3)).toList)
    println("takeWhile(lessThan(4))")
    println(lazyList.takeWhile(lessThan(4)).toList)
    println("takeWhile(isEven)")
    println(lazyList.takeWhile(isEven).toList)
    println("takeWhile(isOdd)")
    println(lazyList.takeWhile(isOdd).toList)
    println("lazyList.headOption")
    println(lazyList.headOption)
    println("emptyList.headOption")
    println(emptyList.headOption)
