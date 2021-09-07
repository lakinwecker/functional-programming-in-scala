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

}
object exercise {
  @main def test =
    val l = List(1,2,3,4)
    println(List.tail(l))
}
