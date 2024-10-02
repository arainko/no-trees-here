package done

import NamedTuple.*

case class FieldTransformer[Name <: String, Source, Dest](name: Name, transformer: Transformer[Source, Dest])

object FieldTransformer {
  given derived[Name <: String, Source, Dest](using
      name: ValueOf[Name],
      transformer: Transformer[Source, Dest]
  ): FieldTransformer[Name, Source, Dest] =
    FieldTransformer(name.value, transformer)

}
