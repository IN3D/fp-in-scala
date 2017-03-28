object Math {
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, prev: Int, curr: Int): Int = {
      if(i == 0) prev
      else go((i - 1), curr, (prev + curr))
    }
    go(n, 0, 1)
  }
}
