import scala.math.Numeric

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List {
  def apply[A](as: A*): List[A] =
    if as.isEmpty then List.Nil
    else List.Cons(as.head, apply(as.tail*))

  def tail[A](list: List[A]): List[A] = list match
    case Cons(_, tail) => tail
    case n => n

  def setHead[A](list: List[A], newHead: A): List[A] = list match
    case Cons(_, tail) => Cons(newHead, tail)
    case Nil => sys.error("Attempting to set the head of an empty list!")

  @annotation.tailrec
  def drop[A](list: List[A], num: Int): List[A] =
    if num <= 0 then list
    else list match
      case Nil => Nil
      case Cons(_, tail) => drop(tail, num-1)

  @annotation.tailrec
  def dropWhile[A](list: List[A], pred: (A) => Boolean): List[A] = list match
      case Nil => Nil
      case Cons(x, tail) if !pred(x) => list
      case Cons(_, tail) => dropWhile(tail, pred)

  def init[A](list: List[A]): List[A] = list match
      case Nil => Nil
      case Cons(x, Nil) => Nil
      case Cons(x, tail) => Cons(x, init(tail))


  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_, len) => len + 1)

  @annotation.tailrec
  def foldLeftOrig[A,B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => foldLeftOrig(xs, f(acc, x), f)

  def foldRightOrig[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRightOrig(xs, acc, f))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeftOrig(List.reverse(as), acc, (b: B, a: A) => f(a, b))

  def foldLeft[A,B](as: List[A], acc: B, f: (B, A) => B): B =
    foldRightOrig(List.reverse(as), acc, (a: A, b: B) => f(b, a))

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0, (acc, _) => acc + 1)

  def sum[A](as: List[A])(implicit num: Numeric[A]): A =
    foldLeft(as, num.zero, num.NumericOps(_) + _)

  def product[A](as: List[A])(implicit num: Numeric[A]): A =
    foldLeft(as, num.one, num.NumericOps(_) * _)

  def subtractLeft[A](as: List[A])(implicit num: Numeric[A]): A =
    foldLeft(as, num.zero, num.NumericOps(_) - _)

  def productLeft[A](as: List[A])(implicit num: Numeric[A]): A =
    foldLeft(as, num.one, num.NumericOps(_) * _)

  def subtractRightOrig[A](as: List[A])(implicit num: Numeric[A]): A =
    foldRightOrig(as, num.zero, num.NumericOps(_) - _)

  def subtractRight[A](as: List[A])(implicit num: Numeric[A]): A =
    foldRight(as, num.zero, num.NumericOps(_) - _)

  def reverse[A](as: List[A]) =
    List.foldLeftOrig(as, List.Nil: List[A], (lhs, rhs) => List.Cons(rhs, lhs))
}

object exercise {
  @main def test =
    val l = List(1,2,3,4)
    println(List.subtractLeft(l))
    println(List.subtractRightOrig(l))
    println(List.subtractRight(l))
    println(List.productLeft(l))
}
