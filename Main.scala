import scala.deriving.Mirror
import NamedTuple.*

case class Builder[
  A,
  ConfiguredFields <: Tuple
](val selector: Selector[A]) {
  private val builder = collection.mutable.Map.empty[String, Any]

  def withField[FieldTpe, Name <: String](f: Selector[A] => wrapper.Field[Name, FieldTpe])(value: FieldTpe): Builder[A, Name *: ConfiguredFields ] = {
    val field = f(selector)
    builder + (field -> value)
    this.copy()
  }
}

object wrapper {

  opaque type Field[Name <: String, A] <: String = String
  
  object Field {
    def apply[A](name: String): Field[name.type, A] = name
  }
}


type MapBoth[X <: AnyNamedTuple, F[_ <: String, _]] =
    NamedTuple[Names[X], Tuple.Map[Tuple.Zip[Names[X], DropNames[X]], [x] =>> x match { case (a, b) => F[a, b] }]]

import wrapper.*

trait Selector[A] extends Selectable {
  type Fields = MapBoth[NamedTuple.From[A], Field]

  def selectDynamic(name: String): Field[name.type, Nothing] = Field(name)
}

def named[A <: Product: Mirror.ProductOf](value: A): NamedTuple[A.MirroredElemLabels, A.MirroredElemTypes] = 
  Tuple.fromProductTyped(value).withNames[A.MirroredElemLabels]

case class Costam(int: Int, str: String)



@main def main = {
  val builder = Builder[Costam, EmptyTuple](new Selector {})

  val sel = new Selector[Costam] {}

  val a = builder.withField(_.int)(1)

  println(a)

  val d = sel.int

  summon["asd" <:< ("asd")]

  // val a: NamedTuple.Names[] = ???



  val cos: (name : Int, bug : Int) = (name = 1, bug = 2)

  val cos2 = named(Costam(1, "asd"))

  // val cos3 = named2[Normal]

  cos2.int

}
