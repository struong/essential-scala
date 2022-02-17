package chapter6

object Exercises {
  val people = Set(
    "Alice",
    "Bob",
    "Charlie",
    "Derek",
    "Edith",
    "Fred")
  val ages = Map(
    "Alice" -> 20,
    "Bob" -> 30,
    "Charlie" -> 50,
    "Derek" -> 40,
    "Edith" -> 10,
    "Fred" -> 60)
  val favoriteColors = Map(
    "Bob" -> "green",
    "Derek" -> "magenta",
    "Fred" -> "yellow")
  val favoriteLolcats = Map(
    "Alice" -> "Long Cat",
    "Charlie" -> "Ceiling Cat",
    "Edith" -> "Cloud Cat")


  def main(args: Array[String]): Unit = {
    def favoriteColour(name: String): String = favoriteColors.getOrElse(name, "beige")

    def printColours(): Unit = for (p <- people) println(s"$p fave colour is ${favoriteColour(p)}")

    favoriteColour("Bob")
    favoriteColour("Charlie")

    printColours()

    def lookup[A](name: String, values: Map[String, A]): Option[A] = values get name

    def oldest = {
      people.foldLeft(Option.empty[String]) {
        case (older, person) =>
          if (ages.getOrElse(person, 0) > older.flatMap(ages.get).getOrElse(0)) {
            Some(person)
          } else {
            older
          }
      }
    }

    for {
      oldest <- oldest
      colour = favoriteColour(oldest)
    } println(colour)


    def intersection[A](left: Set[A], right: Set[A]): Set[A] = {
      left.foldLeft(Set.empty[A]) {
        (accum, current) =>
          if (right.contains(current)) {
            accum + current
          } else {
            accum
          }
      }
    }

    println(intersection(Set(1, 2, 3), Set(2, 3, 4)))

    def union[A](left: Set[A], right: Set[A]): Set[A] = {
      left.foldLeft(right) { (set, element) => (set + element) }
    }

    println(union(Set(1, 2, 3), Set(2, 3, 4)))

    def mapUnion[A](left: Map[A, Int], right: Map[A, Int]) = {
      left.foldLeft(right) {
        case (map, (key, value)) =>
          val total = right.getOrElse(key, 0) + value
          map + (key -> total)
      }
    }

    println(mapUnion(Map('a' ->
      1, 'b' -> 2), Map('a' -> 2, 'b' -> 4)))
  }
}
