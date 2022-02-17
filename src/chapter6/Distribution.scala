package chapter6

final case class Distribution[A](events: List[(A, Double)]) {
  def normalize: Distribution[A] = {
    val totalWeight = (events map { case (a, p) => p }).sum
    Distribution(events map { case (a,p) => a -> (p / totalWeight) })
  }
  def compact: Distribution[A] = {
    val distinct = (events map { case (a, p) => a }).distinct
    def prob(a: A): Double =
      (events filter { case (x, p) => x == a } map { case (a, p) => p
      }).sum
    Distribution(distinct map { a => a -> prob(a) })
  }

  def map[B](f: A => B): Distribution[B] = {
    val newEvents = events map {
      case (d, p) => f(d) -> p
    }
    Distribution(newEvents)
  }

  def flatMap[B](f: A => Distribution[B]): Distribution[B] = {
    Distribution(events flatMap {
      case (a: A, p1) => f(a).events.map {
        case (b, p2) => b -> (p1 * p2)
      }
    }).compact.normalize
  }
}

sealed trait Coin
case object Heads extends Coin
case object Tails extends Coin

object Distribution {
  def uniform[A](data: List[A]): Distribution[A] = {
    val p = 1.0 / data.length
    val events = data.map(a => a -> p)
    new Distribution(events)
  }

  def main(args: Array[String]): Unit = {
    val faircoin: Distribution[Coin] = Distribution.uniform(List(Heads, Tails))

    val threeFlips = for {
      c1 <- faircoin
      c2 <- faircoin
      c3 <- faircoin
    } yield (c1, c2, c3)

    println(s"threeFlips = ${threeFlips}")
  }
}