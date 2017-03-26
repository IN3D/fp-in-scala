case class CreditCard(number: Int) {
  def ==(b: CreditCard) {
    // I think this is right?
    _.number == b.number
  }
}

case class Charge(cc: CreditCard, amount: Double) {

  def combine(other: Charge): Charge =
    if (cc == other.cc)
      Charge(cc, amount + other.amount)
    else
      throw new Exception("Can't combine charges to different cards")

  def coalesce(charges: List[Charge]): List[Charge] =
    // _.cc and (_ combine _) are syntax for anonymous functions
    charges.groupBy(_.cc).values.map(_.reduce(_ combine _)).toList
}

class Cafe {
  def buyCoffee(cc: CreditCard): (Coffee, Charge) = {
    val cup = new Coffee()
    (cup, Charge(cc, cup.price))
  }

  def buyCoffees(cc: CreditCard, n: Int): (List[Coffee], Charge) = {
    // List.fill(n)(x) creates a List with n copies of x. We'll explain this
    // funny function call syntax in a later chapter.
    val purchases: List[(Coffee, Charge)] = List.fill(n)(buyCoffee(cc))
    // unzip splits a list of pairs into a pair of lists. Here we're
    // destructuring this pair to declare two values (coffees, and charges) on
    // one line.
    val (coffees, charges) = purchases.unzip
    (coffees, charges.reduce((c1, c2) => c1.combine(c2)))
    // ^ charges.reduce reduces the entire list of charges to a single charge,
    // using combine to combine charges two at a time. reduce is an example of
    // a higher-order function, which we'll properly introduce in the next
    // chapter.
  }
}
