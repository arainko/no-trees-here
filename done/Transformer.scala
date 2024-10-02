package done

import scala.language.experimental.modularity

// =========== introduced in the 'fancier' section
trait Transformer[Source, Dest] {
  def transform(value: Source): Dest
}

object Transformer {
  given identity[A, B >: A]: Transformer[A, B] with {
    def transform(value: A): A = value
  }
// =========== introduced in the 'fancier' section

  final class Derived[A, B](f: A => B) extends Transformer[A, B] {
    def transform(value: A): B = f(value)
  }
}
