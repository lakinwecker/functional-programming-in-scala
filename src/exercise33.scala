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

}
object exercise {
  @main def test =
    val l = List(1,2,3,4)
    val l2 = List.setHead(l, 5)
    println(l2)
}
