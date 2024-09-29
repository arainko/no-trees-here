package examples

import scala.deriving.Mirror
import scala.compiletime.*

object match_type_examples {
  type ZipAndWrapInOption[A <: Tuple, B <: Tuple] = Tuple.Map[Tuple.Zip[A, B], [a] =>> Option[a]]

  type Result = ZipAndWrapInOption[(Int, String, Double), (List[Int], Map[Int, String], Float)]

  val example: "I'm a literal type" = "I'm a literal type"
  val value = summon[ValueOf["I'm a literal type"]].value // "I'm a literal type"
  val exampleInt: 23 = 23
  // val verified: 23 = 1
}
