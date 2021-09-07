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

}

object exercise {
  @main def test =
    val l = List(1,2,3,4)
    println(List.init(l))
}
