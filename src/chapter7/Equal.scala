package chapter7

import chapter7.Equal.EqualOps


trait Equal[A] {
  def equal(x: A, y: A): Boolean
}

object Equal {
  def apply[A](x: A, y: A)(implicit equal: Equal[A]): Boolean = equal.equal(x, y)
  def apply[A](implicit equal: Equal[A]): Equal[A] = equal // no argument apply version

  implicit class EqualOps[T](left: T) {
    def ===(right: T)(implicit e: Equal[T]): Boolean = {
      e.equal(left, right)
    }
  }
}

object EqualsEmailOnlyImplicit {
  implicit object EqualsEmailOnly extends Equal[Person] {
    override def equal(x: Person, y: Person): Boolean = x.email == y.email
  }
}

object EqualsNameAndEmailImplicit {
  implicit object EqualsNameAndEmail extends Equal[Person] {
    override def equal(x: Person, y: Person): Boolean = x.name == y.name && x.email == y.email
  }
}

object EqualMain {
  implicit val equalsString: Equal[String] = (x: String, y: String) => x.toLowerCase() == y.toLowerCase() // converted to single abstract method

  def main(args: Array[String]): Unit = {
    val equalsNameAndEmail = {
      import chapter7.EqualsNameAndEmailImplicit._
//      Equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
      Equal[Person].equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com")) // no argument apply version
    }

    val equalsName = {
      import chapter7.EqualsEmailOnlyImplicit._
      Equal(Person("Noel", "noel@example.com"), Person("Noel", "noel@example.com"))
    }

    println(s"equalsNameAndEmail = ${equalsNameAndEmail}")
    println(s"equalsName = ${equalsName}")

    println("abcd".===("ABCD"))
  }
}