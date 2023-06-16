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

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] = foldRight(that)(LazyList.cons)

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

  def unfold[A, S](state: S)(f: S => Option[(A, S)]): LazyList[A] = 
    f(state).fold(LazyList.empty)(t => LazyList.cons(t._1, unfold(t._2)(f)))

  def continually[A](a: A): LazyList[A] = unfold(a)(a => Some((a, a)))
  def ones: LazyList[Int] = unfold(99)(_ => Some((1, 99)))
  def from(a: Int): LazyList[Int] = unfold(a)(a => Some((a+1, a+1)))
  def fibsFrom(a: Int, b: Int): LazyList[Int] = unfold((a, b))((a, s) => Some(a, (s, (a+s))))
  def fibs: LazyList[Int] = fibsFrom(0, 1)

  @main
  def main =
    println(continually("lakin").take(13).toList)
    println(from(56).take(13).toList)
    println(ones.take(13).toList)
    println(fibs.take(13).toList)

