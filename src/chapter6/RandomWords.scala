package chapter6

object RandomWords {
  val subjects = List("Noel", "The cat", "The dog")
  val verbs = List("wrote", "chased", "slept on")
  val objects = List("the book", "the ball", "the bed")

  def words: String = {
    val stuff = for {
      s <- subjects
      v <- verbs
      o <- objects
    } yield {
      s"$s $v $o"
    }

    stuff.mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    println(s"words = ${words}")
  }
}


object BetterRandomWords {
  val subjects = List("Noel", "The cat", "The dog")

  def verbsFor(subject: String): Seq[String] = subject match {
   case "Noel" => List("wrote", "chased", "slept on")
   case "The cat" => List("meowed at", "chased", "slept on")
   case "The dog" => List("barked at", "chased", "slept on")
 }

  def objectsFor(verb: String): Seq[String] = verb match {
    case "wrote" => List("the book", "the letter", "the code")
    case "chased" => List("the ball", "the vadog", "the cat")
    case "slept on" => List("the bed", "the mat", "the train")
    case "meowed at" => List("Noel", "the door", "the food cupboard")
    case "barked at" => List("the postman", "the car", "the cat")
  }

  def words: String = {
    val stuff = for {
      s <- subjects
      v <- verbsFor(s)
      o <- objectsFor(v)
    } yield {
      s"$s $v $o"
    }

    stuff.mkString("\n")
  }

  def main(args: Array[String]): Unit = {
    println(s"words = ${words}")
  }
}
