import NamedTuple.*

final class Field[Name <: String, A](val value: Name)

case class Less(int: Int, str: String)
case class More(int: Int, str: String, list: List[String])

object Field {
  type Of[Names <: Tuple, Tpes <: Tuple] =
    Tuple.Map[
      Tuple.Zip[Names, Tpes],
      [x] =>> x match { case (name, tpe) => Field[name, tpe] }
    ]

  type FromPair[Pair] =
    Pair match {
      case (a, b) => Field[a, b]
    }

  type Coerce[f[_ <: String, _]] = [x] =>> x match {
    case Field[name, tpe] => f[name, tpe]
  }

  type ExtractName[field] <: String =
    field match {
      case Field[name, tpe] => name
    }

  type ExtractType[field] =
    field match {
      case Field[name, tpe] => tpe
    }

  type Names[Fields <: Tuple] <: Tuple =
    Fields match {
      case EmptyTuple               => EmptyTuple
      case Field[name, tpe] *: tail => name *: Names[tail]
    }

  type Remove[Name <: String, Fields <: Tuple] <: Tuple =
    Fields match {
      case EmptyTuple               => EmptyTuple
      case Field[Name, tpe] *: tail => Remove[Name, tail]
      case head *: tail             => head *: Remove[Name, tail]
    }

  type Types[Fields <: Tuple] =
    Tuple.Map[Fields, ExtractType]

  type TypeOf[Name, Fields <: Tuple] =
    Fields match {
      case EmptyTuple => Nothing
      case Field[Name, tpe] *: tail => tpe
      case h *: t                   => TypeOf[Name, t]
    }

  type SummonFieldWise[SourceFields <: Tuple, DestFields <: Tuple] =
    DestFields match {
      case EmptyTuple => EmptyTuple
      case _ =>
        Tuple.Map[
          SourceFields,
          [x] =>> x match {
            case Field[srcName, srcTpe] =>
              FieldTransformer[srcName, srcTpe, TypeOf[srcName, DestFields]]
          }
        ]
    }

  type TransformersOf[SourceFields <: Tuple, DestFields <: Tuple] =
    SummonFieldWise[SourceFields, DestFields]

  // type FallibleTransformersOf[SourceFields <: Tuple, DestFields <: Tuple] =
    // SummonFieldWise[SourceFields, DestFields, [src, dest] =>> Transformer.Fallible[src, dest]]

  type NamedOf[Names <: Tuple, Types <: Tuple] =
    NamedTuple[Names, Field.Of[Names, Types]]
}

case class FieldTransformer[Name <: String, Source, Dest](name: Name, transformer: Transformer[Source, Dest])

object FieldTransformer {
  given derived[Name <: String, Source, Dest](using
      name: ValueOf[Name],
      transformer: Transformer[Source, Dest]
  ): FieldTransformer[Name, Source, Dest] =
    FieldTransformer(name.value, transformer)
}
