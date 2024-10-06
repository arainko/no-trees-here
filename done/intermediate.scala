package done

import scala.deriving.Mirror
import scala.compiletime.*

extension [Source <: Product](self: Source) {
  inline def fancierConvertTo[Dest](using
      Source: Mirror.ProductOf[Source],
      Dest: Mirror.ProductOf[Dest]
  ): Dest = {
    type SourceFields = Field.Of[Source.MirroredElemLabels, Source.MirroredElemTypes]
    type DestFields = Field.Of[Dest.MirroredElemLabels, Dest.MirroredElemTypes]

    val erasedSource = self.productElementNames.zip(self.productIterator).toMap

    val transformers =
      summonAll[Field.FieldWiseTransformers[SourceFields, DestFields]].toList.asInstanceOf[List[Transformer[Any, Any]]]

    val destLabels = constValueTuple[Dest.MirroredElemLabels].toList.asInstanceOf[List[String]]

    val transformerMap = destLabels.zip(transformers).toMap
    
    Dest.fromProduct(
      Tuple.fromArray[Any] {
        destLabels.map { label =>
          val sourceValue = erasedSource(label)
          val transformer = transformerMap(label)
          transformer.transform(sourceValue)
        }.toArray
      }
    )
  }
}
