package optics

import monocle.{Iso, Lens, Optional, Prism, Traversal}
import cats.implicits._
import cats.Monoid

object Types {
  case class Name(value: String) extends AnyVal
  case class Stock(value: Int) extends AnyVal

  case class Bar(fridge: Option[Fridge])
  case class Fridge(beers: List[Beer])
  case class Beer(name: Option[Name], stock: Option[Stock])
}

object Instances {
  import Types._
  implicit val stockMonoid: Monoid[Stock] = new Monoid[Stock] {
    override def empty: Stock = Stock(0)
    override def combine(x: Stock, y: Stock): Stock = Stock(x.value + y.value)
  }
}

object BeerOptics {
  import Types._

  val barFridge = Lens[Bar, Option[Fridge]](_.fridge)(newFridge => bar => bar.copy(fridge = newFridge))
  val barFridgeOptional = Optional[Bar, Fridge](_.fridge)(newFridge => bar => bar.copy(fridge = Some(newFridge)))
  val fridgeBeers = Lens[Fridge, List[Beer]](_.beers)(newBeers => fridge => fridge.copy(beers = newBeers))
  val beerStock = Lens[Beer, Option[Stock]](_.stock)(newStock => beer => beer.copy(stock = newStock))
  val beerStockOptional = Optional[Beer, Stock](_.stock)(newStock => beer => beer.copy(stock = Some(newStock)))
  val beerName = Lens[Beer, Option[Name]](_.name)(newName => beer => beer.copy(name = newName))

  val barL = Traversal.fromTraverse[List, Bar]
  val beersL = Traversal.fromTraverse[List, Beer]

  val stockIso = Iso[Stock, Int](_.value)(Stock)

  def prismOption[A]: Prism[Option[A], A] =
    Prism.partial[Option[A], A]{case Some(v) => v}(Some(_))

  val stocksWithOptional: Traversal[List[Bar], Stock] =
    barL.
      composeOptional(barFridgeOptional).
      composeLens(fridgeBeers).
      composeTraversal(beersL).
      composeOptional(beerStockOptional)

  val stocksWithOptionalOperators: Traversal[List[Bar], Stock] =
    barL ^|-? barFridgeOptional ^|-> fridgeBeers ^|->> beersL ^|-? beerStockOptional

  val stocks: Traversal[List[Bar], Stock] =
    barL.
      composeLens(barFridge).
      composePrism(prismOption).
      composeLens(fridgeBeers).
      composeTraversal(beersL).
      composeLens(beerStock).
      composePrism(prismOption)

  val names: Traversal[List[Bar], Name] =
    barL.
      composeLens(barFridge).
      composePrism(prismOption).
      composeLens(fridgeBeers).
      composeTraversal(beersL).
      composeLens(beerName).
      composePrism(prismOption)
}

