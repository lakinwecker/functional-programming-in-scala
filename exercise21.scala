object exercise {
  //------------------------------------------------------------------------------
  // This function doesn't seem to be tailrec compatible. :'(
  //
  // Too bad, I like it more.
  //------------------------------------------------------------------------------
  def nthFibNotTail(n: Int): Int =
    if (n == 1) 0
    else if (n == 2) 1
    else nthFibNotTail(n-1) + nthFibNotTail(n-2)

  def nthFibTail(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, current: Int, next: Int): Int =
      if (n == 0) current
      else go(n-1, next, current+next)
    go(n-1, 0, 1)
  }

  @main def go(n: Int) =
    println(nthFibNotTail(n))
    println(nthFibTail(n))

}
