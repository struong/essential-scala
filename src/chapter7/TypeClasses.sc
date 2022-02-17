import scala.math.Ordering

val minOrdering: Ordering[Int] = Ordering.fromLessThan[Int](_ < _) // type class instances
val maxOrdering: Ordering[Int] = Ordering.fromLessThan[Int](_ > _)

List(3, 4, 2).sorted(minOrdering)
List(3, 4, 2).sorted(maxOrdering)

implicit class ExtraStringMethods(str: String) {
  val vowels = Seq('a', 'e', 'i', 'o', 'u')

  def numberOfVowels = str.toList.count(vowels contains _)
}

"the quick brown fox".numberOfVowels
