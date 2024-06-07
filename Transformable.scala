erased trait Transformable[B] {
  type Self
}

object Transformable {
  // erased given derived[A, B](using erased IntersectionOf[A] <:< IntersectionOf[B]): (A is Transformable[B]) =
  //   compiletime.erasedValue

  type Intersection[Tup <: Tuple] =
    Tuple.Fold[Tup, Any, [acc, curr] =>> acc & curr]

  // type IntersectionOf[A] =
  //   Intersection[NamedTuple.DropNames[Field.Of[A]]]  
}
