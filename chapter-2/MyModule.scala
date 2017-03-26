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
     * functions taht are local to the body of another function. In functional
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
  private def formatAbs(x: Int): String = {
    // A string with two placeholders for numbers marked as %d.
    val msg = "The absolute value of %d is %d"
    // replace the two %d placeholders in the string with x and abs(x)
    // respectively.
    msg.format(x, abs(x))
  }

  // Unit serves the same purpose as void in languages like Java or C.
  def main(args: Array[String]): Unit =
    println(formatAbs(-42))
}
