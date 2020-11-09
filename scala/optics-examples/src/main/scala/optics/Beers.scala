package optics

import monocle.{Iso, Lens, Optional, Prism, Traversal}
import cats.implicits._
import cats.Monoid

object Types {
  case class Name(value: String) extends AnyVal
  case class Stock(value: Int) extends AnyVal
  case class Bar(fridges: List[Fridge])
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

  val barFridges: Lens[Bar, List[Fridge]] = Lens[Bar, List[Fridge]](_.fridges)(newFridges => bar => bar.copy(fridges = newFridges))
  val fridgeBeers = Lens[Fridge, List[Beer]](_.beers)(newBeers => fridge => fridge.copy(beers = newBeers))
  val beerStock = Lens[Beer, Option[Stock]](_.stock)(newStock => beer => beer.copy(stock = newStock))
  val beerStockOptional = Optional[Beer, Stock](_.stock)(newStock => beer => beer.copy(stock = Some(newStock)))
  val beerName = Lens[Beer, Option[Name]](_.name)(newName => beer => beer.copy(name = newName))
  val beerNameOptional = Optional[Beer, Name](_.name)(newName => beer => beer.copy(name = Some(newName)))

  val fridgesL: Traversal[List[Fridge], Fridge] = Traversal.fromTraverse[List, Fridge]
  val barL: Traversal[List[Bar], Bar] = Traversal.fromTraverse[List, Bar]
  val beersL = Traversal.fromTraverse[List, Beer]

  def prismOption[A]: Prism[Option[A], A] =
    Prism.partial[Option[A], A]{case Some(v) => v}(Some(_))

  val barStocks: Traversal[Bar, Stock] =
    barFridges.
      composeTraversal(fridgesL).
      composeLens(fridgeBeers).
      composeTraversal(beersL).
      composeOptional(beerStockOptional)

  val barStocksOperators: Traversal[Bar, Stock] =
    barFridges ^|->> fridgesL ^|-> fridgeBeers ^|->> beersL ^|-? beerStockOptional

  val names: Traversal[Bar, Name] =
    barFridges.
      composeTraversal(fridgesL).
      composeLens(fridgeBeers).
      composeTraversal(beersL).
      composeOptional(beerNameOptional)
}

