import scala.deriving.Mirror
import scala.compiletime.*

object compiletime_examples {
  import scala.compiletime.*

  val summonValues = summonAll[Tuple.Map[("value1", "value2", "value3"), ValueOf]].toList
  val values = summonValues.map(_.value) // List("value1", "value2", "value3")
  val shortened = constValueTuple[("value1", "value2", "value3")].toList // List("value1", "value2", "value3")
}

object mirror_examples {
  case class Example(int: Int, str: String, list: List[Int])
  val mirror: scala.deriving.Mirror.Product {
    type MirroredMonoType = Example; type MirroredType = Example; type MirroredLabel = "Example";
    type MirroredElemTypes = (Int, String, List[Int]); type MirroredElemLabels = ("int", "str", "list")
  } = summon[Mirror.Of[Example]]
  mirror.fromProduct((1, "str", List(1, 2, 3)))
}
