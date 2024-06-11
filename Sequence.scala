type Sequence[T <: Tuple, F[_]] =
  F[Tuple.InverseMap[T, F]]

object Sequence {
  def run[T <: Tuple, F[_]](
      value: T,
      seq: [a] => List[F[a]] => F[List[a]],
      map: [a, b] => (F[a], a => b) => F[b]
  ): Sequence[T, F] = {
    val values = value.productIterator.asInstanceOf[Iterator[F[Any]]].toList
    map(seq(values), list => Tuple.fromArray(list.toArray))
      .asInstanceOf[Sequence[T, F]]
  }

  def runEither[T <: Tuple](
      value: T
  ): Sequence[T, Either[String, _]] = {
    def sequence[A](list: List[Either[String, A]]): Either[String, List[A]] =
      list.foldLeft(Right(List.empty[A]): Either[String, List[A]])(
        (curr, acc) => acc.flatMap(elem => curr.map(list => elem :: list))
      ).map(_.reverse)
    
    run(value, [a] => sequence(_), [a, b] => _.map(_))
  }

  // type a = Tuple.IsMappedBy[]

  type Input = (Option[Int], Option[String], Option[Double], Option[Float])

  type Output = Option[(Int, String, Double, Float)]

  val cos = run[Input, Option](
    (Some(1), Some("Str"), Some(1d), Some(1f)),
    [a] =>
      _.foldLeft(Option(List.empty[a]))((curr, acc) =>
        acc.zip(curr).map(_ :: _)
      ).map(_.reverse),
    [a, b] => _.map(_)
  )
}

@main def main2 = {
  println {
    Sequence.cos.map(_._1)
  }
}
