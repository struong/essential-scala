package chapter7

import java.util.Date

trait HtmlWriter[A] { // the type class
  def write(in: A): String
}

final case class Person(name: String, email: String)


object PersonWriter extends HtmlWriter[Person] {
  override def write(person: Person): String = s"<span>${person.name} &lt;${person.
    email}&gt;</span>"
}

object DateWriter extends HtmlWriter[Date] {
  def write(in: Date) = s"<span>${in.toString}</span>"
}

object ObfuscatedPersonWriter extends HtmlWriter[Person] {
  def write(person: Person) =
    s"<span>${person.name} (${person.email.replaceAll("@", " at ")})</span>"
}

object HtmlUtil {
  def htmlify[A](data: A)(implicit writer: HtmlWriter[A]): String = {
    writer.write(data)
  }
}

object HtmlWriter {
  def write[A](in: A)(implicit writer: HtmlWriter[A]): String =
    writer.write(in)
  def apply[A](implicit writer: HtmlWriter[A]): HtmlWriter[A] = writer // a better version of write
}

object HtmlWriteableMain {

  implicit object ApproximationWriter extends HtmlWriter[Int] {
    override def write(in: Int): String = s"It's definitely less than ${((in / 10) + 1) * 10}"
  }

  def main(args: Array[String]): Unit = {

    val john = Person("John", "john@example.com")
    println(PersonWriter.write(john))
    println(ObfuscatedPersonWriter.write(john))
    println(DateWriter.write(new Date))


    println(HtmlUtil.htmlify(2))

    implicit val writer: ObfuscatedPersonWriter.type = ObfuscatedPersonWriter
    println(HtmlWriter[Person].write(john))
  }
}