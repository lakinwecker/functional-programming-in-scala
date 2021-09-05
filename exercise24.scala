object exercise23 {

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def add(x: Int)(y: Int): Int = x + y

  @main def main() = {
    println(uncurry(add)(1,2))
  }

}
