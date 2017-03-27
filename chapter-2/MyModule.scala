// a comment!
/* Another comment */
/** A documentation comment */

// declares a singleton object, which simultaneously declares a class and its
// only instance.
object MyModule {
  // abs takes an integer, and returns an integer.
  def abs(n: Int): Int = {
    // returns the negation of n if it's less than zero.
    if (n < 0) -n
    else n
  }

  def factorial(n: Int): Int = {
    /* An inner function, or "local definition". It's common in scala to write
     * functions that are local to the body of another function. In functional
     * programming, we shouldn't consider this a bigger deal than local
     * integers or strings.
     */
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n-1, n*acc)

    go(n, 1)
  }

  // A private method can only be called by other members of MyModule.
  private def formatAbs(x: Int): String =
    formatResult("absolute value", x, abs)

  private def formatFactorial(x: Int): String =
    formatResult("factorial", x, factorial)

  private def formatResult(name: String, n: Int, f: Int => Int): String = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  // Unit serves the same purpose as void in languages like Java or C.
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}
