val absOrdering = Ordering.fromLessThan[Int] { (x, y) =>
  Math.abs(x) < Math.abs(y)
 }

assert(List(-4, -1, 0, 2, 3).sorted(absOrdering) == List(0, -1, 2, 3,
  -4))
assert(List(-4, -3, -2, -1).sorted(absOrdering) == List(-1, -2, -3,
  -4))

implicit val absOrdering2: Ordering[Int] = Ordering.fromLessThan[Int] { (x, y) =>
  Math.abs(x) < Math.abs(y)
}

assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3,
  -4))
assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3,
  -4))

final case class Rational(numerator: Int, denominator: Int)

implicit val rationalOrdering: Ordering[Rational] = Ordering.fromLessThan[Rational] { (x, y) =>
  if(x.numerator == y.numerator) {
    x.denominator > y.denominator
  } else {
    x.numerator < y.numerator
  }
}

assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted ==
  List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))

implicit class IntOps(kool: Int) {
  def yeah(): Unit = {
    times(_ => println("Oh yeah!"))
  }

  def times(f: Int => Unit): Unit = {
    for(_ <- 0 until kool) f(kool)
  }
}

2.yeah()
3.yeah()
(-1).yeah()

3.times(i => println(s"Look - it's the number $i!"))
