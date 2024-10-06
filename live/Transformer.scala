package live

@FunctionalInterface
trait Transformer[Source, Dest] {
  def transform(value: Source): Dest
}

object Transformer {
  given identity[A]: Transformer[A, A] = a => a
}
