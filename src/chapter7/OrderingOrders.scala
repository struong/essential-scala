package chapter7

final case class Order(units: Int, unitPrice: Double) {
  val totalPrice: Double = units * unitPrice
}

object Order {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan { (x, y) =>
    x.totalPrice < y.totalPrice
  }
}

object UnitsOrdering {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan { (x, y) =>
    x.units < y.units
  }
}

object UnitPriceOrdering {
  implicit val ordering: Ordering[Order] = Ordering.fromLessThan { (x, y) =>
    x.unitPrice < y.unitPrice
  }
}

object OrderingOrders {
  def main(args: Array[String]): Unit = {

    val orders = List(
      Order(2, 0.5),
      Order(5, 5),
      Order(1, 0.5)
    )

    println(s"orders.sorted = ${orders.sorted}")

  }
}
