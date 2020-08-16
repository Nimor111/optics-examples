package optics

import cats.Monoid
import cats.implicits._

object Main extends App {
  import BeerOptics._
  import Types._
  import Instances._

  val beer1 = Beer(Some(Name("Corona Extra")), Some(Stock(4)))
  val beer2 = Beer(None, Some(Stock(3)))
  val bars = List(
    Bar(
      Some(Fridge(List(beer1, beer2)))
    ),
    Bar(
      None
    ))

  println(stocksWithOptionalOperators.fold(bars))
  println(stocksWithOptionalOperators.fold(stocksWithOptionalOperators.modify(s => Stock(s.value + 1))(bars)))

  println(names.getAll(names.modify(n => Name(n.value + "!"))(bars)))
}
