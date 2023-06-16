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
    if (n <= 0) Empty
    else this match
      case Empty => this
      case Cons(h, rest) => Cons(h, () => rest().take(n-1))

  def drop(n: Int): LazyList[A] = 
    if (n <= 0) this
    else this match
      case Empty => this
      case Cons(h, rest) => rest().drop(n-1)


object LazyList:
  def cons[A](hd: => A, tl: LazyList[A]): LazyList[A] =
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)

  def empty[A]: LazyList[A] = Empty

  def apply[A](as: A*): LazyList[A] =
    if as.isEmpty then empty
    else cons(as.head, apply(as.tail*))



  @main
  def main =
    val lazyList = LazyList(1, 2, 3, 4, 5)
    println(lazyList.toList)
    println(lazyList.take(2).toList)
    println(lazyList.drop(2).toList)
