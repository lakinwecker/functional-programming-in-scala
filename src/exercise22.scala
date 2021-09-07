object exercise22 {
  def isSorted[A](items: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(i0: Int): Boolean = 
      if (i0 + 1 >= items.length) true
      else if (!gt(items(i0), items(i0+1))) false
      else go(i0+1)

    go(0)
  }

  @main def main() = {
    val numbers = Array(1,2,3,4)
    println(isSorted(numbers, (a: Int, b: Int) => a > b))
    println(isSorted(numbers.reverse, (a: Int, b: Int) => a > b))
    val oneNumber = Array(1)
    println(isSorted(oneNumber, (a: Int, b: Int) => a > b))
  }

}
