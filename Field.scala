import NamedTuple.*

opaque type Field[Name <: String, +A] = String

case class Less(int: Int, str: String)
case class More(int: Int, str: CharSequence, list: List[String])

object Field {
  def apply[A](name: String): Field[name.type, A] = name

  type MapBoth[X <: AnyNamedTuple, F[_ <: String, _]] =
    NamedTuple[Names[X], Tuple.Map[
      Tuple.Zip[Names[X], DropNames[X]],
      [x] =>> x match { case (a, b) => F[a, b] }
    ]]

  type Of[A] = MapBoth[NamedTuple.From[A], Field]
}

object ops {
  type Intersection[Tup <: Tuple] =
    Tuple.Fold[Tup, Any, [acc, curr] =>> acc & curr]

  type IntersectionOf[A] =
    Intersection[NamedTuple.DropNames[Field.Of[A]]]

  def canTransform[A, B](using IntersectionOf[A] <:< IntersectionOf[B]) = ???

  // summon[IntersectionOf[Less] =:= IntersectionOf[More]]

  canTransform[More, Less]
}
