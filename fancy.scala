import scala.deriving.Mirror
import scala.compiletime.*

case class Source(int: Int, str: String, list: List[Int], additional: Double)
case class Dest(list: List[Int], int: Int, str: String)

type Intersection[A <: Tuple] = Tuple.Fold[A, Any, [a, b] =>> a & b]

val src = Source(1, "str", List(1, 2, 3), 1d)

val dest = src.convertTo[Dest]

extension [Source <: Product](self: Source) {
  inline def convertTo[Dest](using
      Source: Mirror.ProductOf[Source],
      Dest: Mirror.ProductOf[Dest]
  )(using
      ev: Intersection[Field.Of[Source.MirroredElemLabels, Source.MirroredElemTypes]] <:<
        Intersection[Field.Of[Dest.MirroredElemLabels, Dest.MirroredElemTypes]]
  ): Dest = {
    val erasedSource = self.productElementNames.zip(self.productIterator).toMap
    val destLabels = summonAll[Tuple.Map[Dest.MirroredElemLabels, ValueOf]].toList
      .asInstanceOf[List[ValueOf[String]]]
    Dest.fromProduct(
      Tuple.fromArray[Any](destLabels.map(label => erasedSource(label.value)).toArray)
    )
  }
}
