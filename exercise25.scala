object exercise25 {

  def compose[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))

  def toString(x: Int): String = x.toString()
  def firstChar(x: String): Char = x(0)

  @main def main() = {
    println(toString(200))
    println(compose(toString, firstChar)(200))
  }

}
