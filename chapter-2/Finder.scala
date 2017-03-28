object Finder {
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def go(n: Int): Int =
      if (n >= as.length) -1
      else if (p(as(n))) n
      else go(n + 1)

    go(0)
  }

  def partial1[A,B,C](a: A, f: (A,B) => C): B => C = (b: B) => f(a, b)
}
