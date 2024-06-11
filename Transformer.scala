import scala.language.experimental.modularity
import scala.deriving.Mirror
import scala.compiletime.*

trait Transformer[Dest] {
  type Self

  def transform(value: Self): Dest
}

object Transformer {
  given identity[A]: (A has Transformer[A]) with {
    def transform(value: Self): A = value
  }

  trait Fallible[Dest] extends Transformer[Either[String, Dest]]

  object Fallible {
    given identity[A, B >: A]: (A has Transformer.Fallible[B]) with {
      def transform(value: Self): Either[String, B] = Right(value)
    }
  }

  final class Derived[A, B](f: A => B) extends (A has Transformer[B]) {
    def transform(value: Self): B = f(value)
  }
}
