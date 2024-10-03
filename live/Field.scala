package live

class Field[Name <: String, A]

object Field {
  type Of[Labels <: Tuple, Types <: Tuple] =
    Tuple.Map[
      Tuple.Zip[Labels, Types],
      [a] =>> a match { case (label, tpe) => Field[label, tpe] }
    ]
}
