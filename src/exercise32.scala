enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List {
  def apply[A](as: A*): List[A] =
    if as.isEmpty then List.Nil
    else List.Cons(as.head, apply(as.tail*))

  def tail[A](list: List[A]): List[A] = list match
    case Cons(_, tail) => tail
    case Nil => Nil

}
object exercise {
  @main def test =
    println(List.tail(List.Nil))
    println(List.tail(List(1)))
    println(List.tail(List(1, 2)))
    println(List.tail(List(1, 2, 3)))
}
