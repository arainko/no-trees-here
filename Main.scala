import scala.deriving.Mirror
import scala.compiletime.ops.any.IsConst
import scala.annotation.compileTimeOnly

enum Config[-Source, +FieldTpe] {
  case Const(value: FieldTpe)
  case Computed(function: Source => FieldTpe)
}


case class Builder[
    Source,
    Dest,
    SourceFields <: Tuple,
    DestFields <: Tuple,
    DestNames <: Tuple,
    DestTypes <: Tuple,
    ConfiguredFields <: Tuple
] private (private val configs: Map[String, Config[?, ?]]) {
  private val selector = Selector.of[DestNames, DestTypes]

  // type WithConfig[Name <: String, Tpe] = Builder[Source, Dest, Field[Name, Tpe] *: ConfiguredFields]

  def withField[Tpe, Name <: String](
      f: Selector[DestNames, DestTypes] => Field[Name, Tpe]
  )(config: Config[Source, Tpe]) = {
    val field = f(selector)
    this.copy(configs = configs + (field.toString -> config))
  }

  inline def transform: Dest = {
    // compiletime.summonAll[Tuple.Map[DestNames, [name] =>> Field.TypeOf[name, DestFields]]]
    compiletime.summonAll[Field.TransformersOf[SourceFields, DestFields]]
    // compiletime.summonInline[Field.TypeOf[Tuple.Head[DestNames], DestFields]]
    ???
  }
}

object Builder {
  def create[Source: Mirror.ProductOf, Dest: Mirror.ProductOf]: Builder[
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
    Dest.MirroredElemLabels,
    Dest.MirroredElemTypes,
    EmptyTuple
  ] = Builder(Map.empty)
}

import NamedTuple.*

case class Costam(int: Int, str: String)
case class Costam2(int: Int, str: String)

@main def main = {

  val builder = Builder.create[Costam, Costam2]

  val M = summon[Mirror.Of[Costam]]

  val sel = Selector.of[M.MirroredElemLabels, M.MirroredElemTypes]
  // given Int = 1

  // given String = ""

  // given List[String] = Nil



  val a =
    builder
      // .withField(_.int)(Config.Const(1))
      .transform





    // .withField(_.int)(Config.Const(1))
    // .withField(_.str)(Config.Computed(a => a.str))
    // .withField(_.list)(Config.Const(Nil))
    // .transform


  println(a)


  val d = sel.str

  summon["asd" <:< ("asd")]

  // val a: NamedTuple.Names[] = ???

  val cos: (name: Int, bug: Int) = (name = 1, bug = 2)

  // val cos3 = named2[Normal]
}
