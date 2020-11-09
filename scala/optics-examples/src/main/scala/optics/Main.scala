package optics

import cats.Monoid
import cats.implicits._

object Simple {
  abstract class Lens[S, T, A, B] {
    def get(s: S): A
    def set(s: S, b: B): T
    def modify(s: S)(f: A => B): T = set(s, f(get(s)))
  }

  type SimpleLens[S, A] = Lens[S, S, A, A]
  case class Name(value: String)

  case class Beer(name: Name)

  // We focus on the field with type Name of the Beer class
  val beerName = new SimpleLens[Beer, Name] {
    def get(s: Beer): Name = s.name
    def set(s: Beer, newName: Name): Beer = s.copy(name = newName)
  }
}

object Main extends App {
  import BeerOptics._
  import Types._
  import Instances._

  val firstFridgeBeer1 = Beer(Some(Name("Starobrno")), Some(Stock(5)))
  val firstFridgeBeer2 = Beer(Some(Name("")), Some(Stock(2)))
  val secondFridgeBeer1 = Beer(Some(Name("Starobrno")), None)
  val secondFridgeBeer2 = Beer(Some(Name("Staropramen")), Some(Stock(6)))

  val fridges = List(
    Fridge(List(firstFridgeBeer1, firstFridgeBeer2)),
    Fridge(List(secondFridgeBeer1, secondFridgeBeer2)))
  val bar = Bar(fridges)

  println(barStocksOperators.fold(bar))
  println(barStocksOperators.fold(barStocksOperators.modify(s => Stock(s.value + 1))(bar)))
}
