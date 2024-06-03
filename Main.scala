import scala.deriving.Mirror
import scala.compiletime.ops.any.IsConst

enum Config[-Source, +FieldTpe] {
  case Const(value: FieldTpe)
  case Computed(function: Source => FieldTpe)
}

import Transformable.*

case class Builder[
  Source,
  Dest,
  ConfiguredFields <: Tuple
] private (private val configs: Map[String, Config[?, ?]]) {
  private val selector = Selector.of[Dest]

  type WithConfig[Name <: String, Tpe] = Builder[Source, Dest, Field[Name, Tpe] *: ConfiguredFields]

  def withField[Tpe, Name <: String](f: Selector[Dest] => Field[Name, Tpe])(config: Config[Source, Tpe]): WithConfig[Name, Tpe] = {
    val field = f(selector)
    this.copy(configs = configs + (field.toString -> config))
  }

  def transform(using erased (IntersectionOf[Source] & Intersection[ConfiguredFields]) <:< IntersectionOf[Dest]): Dest = ???
}

object Builder {
  def create[Source, Dest]: Builder[Source, Dest, EmptyTuple] = Builder(Map.empty)
}

import NamedTuple.*


type MapBoth[X <: AnyNamedTuple, F[_ <: String, _]] =
    NamedTuple[Names[X], Tuple.Map[Tuple.Zip[Names[X], DropNames[X]], [x] =>> x match { case (a, b) => F[a, b] }]]

def named[A <: Product: Mirror.ProductOf](value: A): NamedTuple[A.MirroredElemLabels, A.MirroredElemTypes] = 
  Tuple.fromProductTyped(value).withNames[A.MirroredElemLabels]

case class Costam(int: Int, str: String)




@main def main = {
  val builder = Builder.create[Less, More]

  val sel = Selector.of[Costam]

  val a = 
    builder
      // .withField(_.int)(Config.Const(1))
      // .withField(_.str)(Config.Computed(a => a.str))
      .withField(_.list)(Config.Const(Nil))
      .transform

  println(a)

  val d = sel.str

  summon["asd" <:< ("asd")]

  // val a: NamedTuple.Names[] = ???



  val cos: (name : Int, bug : Int) = (name = 1, bug = 2)

  val cos2 = named(Costam(1, "asd"))

  // val cos3 = named2[Normal]

  cos2.int

}
