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
  def foldLeft[A,B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => foldLeft(xs, f(acc, x), f)

  def foldRightOrig[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    as match
      case Nil => acc
      case Cons(x, xs) => f(x, foldRight(xs, acc, f))

  def foldRight[A,B](as: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(List.reverse(as), acc, (b: B, a: A) => f(a, b))

  def length2[A](as: List[A]): Int =
    foldLeft(as, 0, (acc, _) => acc + 1)

  def sum[A](as: List[A])(implicit num: Numeric[A]): A =
    foldLeft(as, num.zero, num.NumericOps(_) + _)

  def product[A](as: List[A])(implicit num: Numeric[A]): A =
    foldLeft(as, num.one, num.NumericOps(_) * _)

  def subtractLeft[A](as: List[A])(implicit num: Numeric[A]): A =
    foldLeft(as, num.zero, num.NumericOps(_) - _)

  def subtractRightOrig[A](as: List[A])(implicit num: Numeric[A]): A =
    foldRightOrig(as, num.zero, num.NumericOps(_) - _)

  def subtractRight[A](as: List[A])(implicit num: Numeric[A]): A =
    foldRight(as, num.zero, num.NumericOps(_) - _)

  def reverse[A](as: List[A]) =
    List.foldLeft(as, List.Nil: List[A], (lhs, rhs) => List.Cons(rhs, lhs))

  def concat[A](a1: List[A], a2: List[A]): List[A] =
    List.foldRight(a1, a2, (elem, as) => List.Cons(elem, as))

  def flatten[A](as: List[List[A]]): List[A] =
    List.foldLeft(as, Nil: List[A], concat)

  def concatFoldRight[A](as: List[List[A]]): List[A] =
    List.foldRight(as, List.Nil: List[A], concat)

  def add1[A](as: List[A])(implicit num: Numeric[A]): List[A] =
    List.foldRight(as, Nil: List[A], (elem, newList) => Cons(num.NumericOps(elem)+num.one, newList))

  def toString[A](as: List[A]): List[String] =
    List.foldRight(as, Nil: List[String], (elem, newList) => Cons(elem.toString, newList))

  def map[A, B](f: A => B)(as: List[A]): List[B] =
    List.foldRight(as, Nil: List[B], (elem, newList) => Cons(f(elem), newList))

  def filterOld[A](f: A => Boolean)(as: List[A]): List[A] =
    List.foldRight(as, Nil: List[A], (elem, newList) => if (f(elem)) Cons(elem, newList) else newList)

  def flatMap[A, B](f: A => List[B])(as: List[A]): List[B] =
    List.flatten(List.foldRight(as, Nil: List[List[B]], (elem, newList) => Cons(f(elem), newList)))

  def filter[A](f: A => Boolean)(as: List[A]): List[A] =
    List.flatMap[A, A](elem => if (f(elem)) List(elem) else Nil)(as)

  def zipSum(as1: List[Int], as2: List[Int]): List[Int] = (as1, as2) match 
    case (Cons(x, Nil), Cons(y, Nil)) => List(x + y)
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, zipSum(xs, ys))
    case _ => Nil

  def zipMap[A, B](f: (A, A) => B)(as1: List[A], as2: List[A]): List[B] = (as1, as2) match 
    case (Cons(x, Nil), Cons(y, Nil)) => List(f(x, y))
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipMap(f)(xs, ys))
    case _ => Nil

  def all(as: List[Boolean]): Boolean = List.foldLeft(as, true, _ && _)

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = 
      (List.length(sup) >= List.length(sub)) &&
      List.all(List.zipMap[A, Boolean](_ == _)(sup, sub))

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    startsWith(sup, sub) || (sup match {
      case Cons(x, t) => hasSubsequence(t, sub)
      case _ => false
    })
  }

}

object exercise {
  @main def test =
    val l1 = List(1, 2, 3, 4)
    val l2 = List(5, 6, 7, 8)
    println(List.hasSubsequence(l1, l2))
    println(List.hasSubsequence(l1, List(4, 3)))
    println(List.hasSubsequence(l1, List(2, 1)))
    println(List.hasSubsequence(l1, List(2, 3)))
    println(List.hasSubsequence(l1, List(1, 2)))
    println(List.hasSubsequence(l1, List(1)))
    println(List.hasSubsequence(l1, List(3, 4)))
}

