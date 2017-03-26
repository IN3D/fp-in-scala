object Math {
  def fibonacci(n: Int): Int = {
    @annotation.tailrec
    def go(i: Int, curr: Int, prev: Int):Int = {
      if(n == 0 || n == 1) n
      else if(i == n) curr
      else go((i + 1), (curr + prev), curr)
    }
    go(1, 1, 0)
  }
}
