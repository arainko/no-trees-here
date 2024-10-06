package done

import scala.compiletime.*
import scala.deriving.Mirror
import java.time.LocalDate

case class Builder[
    Source <: Product,
    Dest,
    SourceFields <: Tuple,
    TransformedDestFields <: Tuple,
    ConfiguredDestFields <: Tuple,
    DestNames <: Tuple,
    DestTypes <: Tuple
] private (private val source: Source, private val configs: Map[String, Config[Any, Any]]) {
  private val selector = Selector.of[DestNames, DestTypes]

  type WithConfig[Name <: String, Tpe] =
    Builder[
      Source,
      Dest,
      SourceFields,
      Field.Remove[Name, TransformedDestFields],
      Field[Name, Tpe] *: ConfiguredDestFields,
      DestNames,
      DestTypes
    ]

  def withField[Tpe, Name <: String](
      select: Selector[DestNames, DestTypes] => Field[Name, Tpe]
  )(config: Config[Source, Tpe]): WithConfig[Name, Tpe] = {
    val field = select(selector)
    this.copy(configs = configs + (field.value -> config.asInstanceOf[Config[Any, Any]]))
  }

  inline def transform(using Dest: Mirror.ProductOf[Dest]): Dest = {
    val fieldTransformers =
      summonAll[Field.TransformersOf[SourceFields, TransformedDestFields]].toList
        .asInstanceOf[List[FieldTransformer[String, Any, Any]]]
        .map(fieldTransformer => fieldTransformer.name -> fieldTransformer.transformer)
        .toMap

    val destLabels = constValueTuple[DestNames].toList.asInstanceOf[List[String]]

    val erasedSource =
      source.productElementNames.zip(source.productIterator).toMap

    val destValues = destLabels.map { label =>
      fieldTransformers
        .get(label)
        .map(transformer => transformer.transform(erasedSource(label)))
        .getOrElse {
          configs(label) match
            case Config.Const(value)       => value
            case Config.Computed(function) => function(source)
        }
    }

    Dest.fromProduct(Tuple.fromArray[Any](destValues.toArray))
  }
}

object Builder {
  def create[Source <: Product: Mirror.ProductOf, Dest: Mirror.ProductOf](source: Source): Builder[
    Source,
    Dest,
    Tuple.Map[
      Tuple.Zip[Source.MirroredElemLabels, Source.MirroredElemTypes],
      Field.FromPair
    ],
    Tuple.Map[
      Tuple.Zip[Dest.MirroredElemLabels, Dest.MirroredElemTypes],
      Field.FromPair
    ],
    EmptyTuple,
    Dest.MirroredElemLabels,
    Dest.MirroredElemTypes
  ] = Builder(source, Map.empty)
}

extension [Source <: Product](self: Source) {
  def into[Dest](using Mirror.ProductOf[Source], Mirror.ProductOf[Dest]) =
    Builder.create[Source, Dest](self)
}

@main def fancyTest = {
  case class Album(
      name: String,
      artist: String,
      releaseDate: LocalDate
  )

  case class EverSoSlightlyMoreDetailedAlbum(
      releaseDate: LocalDate,
      artist: String,
      name: String,
      numberOfTracks: Int,
      label: String
  )

  val detailedAlbum =
    EverSoSlightlyMoreDetailedAlbum(
      releaseDate = LocalDate.of(2024, 9, 27),
      artist = "Xiu Xiu",
      name = "13'' Frank Beltrame Italian Stiletto with Bison Horn Grips",
      numberOfTracks = 9,
      label = "Polyvinyl Record Co."
    )

  val album = Album(
    name = "13'' Frank Beltrame Italian Stiletto with Bison Horn Grips",
    artist = "Xiu Xiu",
    releaseDate = LocalDate.of(2024, 9, 27)
  )

  val detailedToAlbum = detailedAlbum.convertTo[Album] // works!

  val albumToDetailed = album
    .into[EverSoSlightlyMoreDetailedAlbum]
    .withField(_.numberOfTracks)(Config.Const(9))
    .withField(_.label)(Config.Const("Polyvinyl Record Co."))
    .transform // works!

  println(albumToDetailed)
  println(detailedToAlbum)

  assert(album == detailedToAlbum)
  assert(detailedAlbum == albumToDetailed)
}
