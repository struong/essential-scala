package chapter7

import java.util.Date

sealed trait JsValue {
  def stringify: String
}

final case class JsObject(values: Map[String, JsValue]) extends JsValue {
  override def stringify: String = values.map {
    case (name, value) => "\"" + name + "\":" + value.stringify
  }.mkString("{", ",", "}")
}

final case class JsString(value: String) extends JsValue {
  override def stringify: String = "\"" + value.replaceAll("\\|\"", "\\\\$1") + "\""
}

sealed trait Visitor {
  def id: String

  def createdAt: Date

  def age: Long = new Date().getTime - createdAt.getTime
}

final case class Anonymous(
                            id: String,
                            createdAt: Date = new Date()
                          ) extends Visitor

final case class User(
                       id: String,
                       email: String,
                       createdAt: Date = new Date()
                     ) extends Visitor

// type class for converting Scala data to Json
trait JsWriter[T] {
  def write(value: T): JsValue
}


object JsonSerialisation {
  // dispatch part of our type class
  implicit class JsUtil[T](value: T) {
    def toJson(implicit jsWriter: JsWriter[T]): JsValue = jsWriter.write(value)
  }

  implicit object StringWriter extends JsWriter[String] {
    override def write(value: String): JsValue = JsString(value)
  }

  implicit object DateWriter extends JsWriter[Date] {
    override def write(value: Date): JsValue = JsString(value.toString)
  }

  implicit object AnonymousJsWriter extends JsWriter[Anonymous] {
    override def write(value: Anonymous): JsValue = JsObject(
      Map(
        "id" -> value.id.toJson,
        "createdAt" -> value.createdAt.toString.toJson
      )
    )
  }

  implicit object UserJsWriter extends JsWriter[User] {
    override def write(value: User): JsValue = JsObject(
      Map(
        "id" -> value.id.toJson,
        "email" -> value.email.toJson,
        "createdAt" -> value.createdAt.toString.toJson
      )
    )
  }

  implicit object VisitorWriter extends JsWriter[Visitor] {
    override def write(value: Visitor): JsValue = value match {
      case anonymous: Anonymous => anonymous.toJson
      case user: User => user.toJson
    }
  }

  def main(args: Array[String]): Unit = {
    val obj = JsObject(
      Map(
        "foo" -> JsString("a"),
        "bar" -> JsString("b"),
        "baz" -> JsString("c")
      )
    )

    println(obj.stringify)

    val anonymous = Anonymous("anon-id")
    println(anonymous.toJson.stringify)

    val user = User("user-id", "user-email")
    println(user.toJson.stringify)

    val visitors: Seq[Visitor] = Seq(anonymous, user)
    visitors.foreach(v => println(v.toJson.stringify))
  }
}
