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

  def map[B](f: A => B): LazyList[B] = foldRight[LazyList[B]](LazyList.Empty)((a, b) => LazyList.cons(f(a), b))

  def filter[B](p: A => Boolean): LazyList[A] = foldRight[LazyList[A]](LazyList.Empty)((a, b) => if p(a) then LazyList.cons(a, b) else b)


  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] = foldRight[LazyList[A2]](that)(LazyList.cons)
  def flatMap[B](f: A => LazyList[B]): LazyList[B] = foldRight[LazyList[B]](LazyList.Empty)((a, b) => f(a).append(b))

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
  def squareAndCube(v: Int): LazyList[Int] = LazyList.cons(v*v, LazyList.cons(v*v*v, Empty))

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
    val lazyList2 =
      LazyList.cons(
        lazyMake(8),
        LazyList.cons(lazyMake(9), Empty)
    )
    println("lazyList.takeWhile(isOdd)")
    println(lazyList.takeWhile(isOdd))
    println("lazyList.takeWhile(isOdd).toList")
    println(lazyList.takeWhile(isOdd).toList)
    println("lazyList.filter(isEven)")
    println(lazyList.filter(isEven))
    println("lazyList.filter(isEven).toList")
    println(lazyList.filter(isEven).toList)
    println("lazyList.append(lazyMake(6)).toList")
    println(lazyList.append(lazyList2).toList)
    println("lazyList.flatMap(squareAndCube).toList")
    println(lazyList.flatMap(squareAndCube).toList)
