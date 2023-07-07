import scala.util.control.NonFatal

enum LazyList[+A]:
  case Empty
  case Cons(h: () => A, t: () => LazyList[A])


  def toList: List[A] = this match 
    case Empty => List()
    case Cons(h, rest) => List(h()) ++ rest().toList


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

  def headOption: Option[A] = foldRight[Option[A]](None)((a, b) => Some(a))

  def map[B](f: A => B): LazyList[B] =
    LazyList.unfold[B, LazyList[A]](this)({
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    })

  def take(n: Int): LazyList[A] = 
    LazyList.unfold((0, this))({
      case (count, Cons(h, t)) if count < n => Some((h(), (count+1, t())))
      case _ => None
    })

  def takeWhile(p: A => Boolean): LazyList[A] =
    LazyList.unfold(this)({
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    })

  def zipWith[B, C](b: LazyList[B], f: (A, B) => C) =
    LazyList.unfold((this, b))({case (a, b) => (a, b) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Some(f(ha(), hb()), (ta(), tb()))
      case _ => None
    }})

  def zipAll[B, C](b: LazyList[B]): LazyList[(Option[A], Option[B])] =
    LazyList.unfold((this, b))({case (a, b) => (a, b) match {
      case (Cons(ha, ta), Cons(hb, tb)) => Some((Some(ha()), Some(hb())), (ta(), tb()))
      case (Cons(ha, ta), Empty) => Some((Some(ha()), None), (ta(), Empty))
      case (Empty, Cons(hb, tb)) => Some((None, Some(hb())), (Empty, tb()))
      case _ => None
    }})

  def startsWith[A2 >: A](prefix: LazyList[A2]): Boolean =
    zipAll(prefix).forAll({
      case (Some(a), Some(p)) => a == p
      case (Some(a), None) => true
      case (None, _) => false
    })

  def filter[B](p: A => Boolean): LazyList[A] = foldRight[LazyList[A]](LazyList.Empty)((a, b) => if p(a) then LazyList.cons(a, b) else b)

  def append[A2 >: A](that: => LazyList[A2]): LazyList[A2] = foldRight(that)(LazyList.cons)

  def flatMap[B](f: A => LazyList[B]): LazyList[B] = foldRight[LazyList[B]](LazyList.Empty)((a, b) => f(a).append(b))

  // NOTE: it feels like I am evaluating this WAY too often, but not sure how to avoid?
  // TODO: consider testing the above thought
  def tails: LazyList[LazyList[A]] =
    LazyList.unfold(Option(this))(l => l match {
      case Some(Cons(ha, ta)) => {
        val tail = ta()
        Some(Cons(ha, () => tail), Some(tail))
      }
      case _ => None
    }).append(LazyList.empty)

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
    println(LazyList(1, 2, 3).tails.map(_.toList).toList)

