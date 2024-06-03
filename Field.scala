import NamedTuple.*

opaque type Field[Name <: String, A] = String

case class Less(int: Int, str: String)
case class More(int: Int, str: String, list: List[String])

object Field {
  def apply[A](name: String): Field[name.type, A] = name

  type MapBoth[X <: AnyNamedTuple, F[_ <: String, _]] =
    NamedTuple[Names[X], Tuple.Map[
      Tuple.Zip[Names[X], DropNames[X]],
      [x] =>> x match { case (a, b) => F[a, b] }
    ]]

  type Of[A] = MapBoth[NamedTuple.From[A], Field]
}
