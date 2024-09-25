import scala.deriving.Mirror
import scala.compiletime.*

val dest1 =
  io.github.arainko.ducktape.Transformer.Debug.showCode {
    src.fancierConvertTo[Dest]
  }

extension [Source <: Product](self: Source) {
  inline def fancierConvertTo[Dest](using
      Source: Mirror.ProductOf[Source],
      Dest: Mirror.ProductOf[Dest]
  ): Dest = {
    type SourceFields = Field.Of[Source.MirroredElemLabels, Source.MirroredElemTypes]
    type DestFields = Field.Of[Dest.MirroredElemLabels, Dest.MirroredElemTypes]

    val transformers =
      summonAll[Field.TransformersOf[SourceFields, DestFields]].toList
        .asInstanceOf[List[FieldTransformer[String, Any, Any]]]
        .map(fieldTransformer => fieldTransformer.name -> fieldTransformer)
        .toMap

    val destLabels = summonAll[Tuple.Map[Dest.MirroredElemLabels, ValueOf]].toList
      .asInstanceOf[List[ValueOf[String]]]

    val erasedSource = self.productElementNames.zip(self.productIterator).toMap

    Dest.fromProduct(
      Tuple.fromArray[Any](destLabels.map(label => transformers(label.value).transformer.transform(erasedSource(label.value))).toArray)
    )
  }
}
