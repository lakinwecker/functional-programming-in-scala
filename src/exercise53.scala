import scala.util.control.NonFatal

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])

  def headOption: Option[A] = this match
    case Empty => None
    case Cons(h, _) => Some(h())

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

  def takeWhile(p: A => Boolean): LazyList[A] = 
    this match
      case Empty => this
      case Cons(h, rest) =>
        lazy val predicate = () => p(h())
        if(predicate()) LazyList.cons(h(), rest().takeWhile(p))
        else LazyList.empty


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
    println(s"Making: ${a}")
    a

  @main
  def main =
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
    // println("beforeToList")
    // println(lazyList.toList)
    // println("afterToList")
    println("before all")
    val take2 = lazyList.take(4)
    val drop2 = take2.drop(2)
    println(1)
    val takeWhile = take2.takeWhile((a) => a < 2)
    println(2)
    println("after all")
    println(takeWhile.toList)
    //println("before drop2")
    //println("after drop2")
    //println(drop2.toList)
    //println(lazyList.drop(2).toList)
    //println(lazyList.takeWhile((a) => a < 4).toList)
