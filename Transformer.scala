import scala.language.experimental.modularity
import scala.deriving.Mirror
import scala.compiletime.*

trait Transformer[Source, Dest] {
  def transform(value: Source): Dest
}

object Transformer {
  given identity[A, B >: A]: Transformer[A, B] with {
    def transform(value: A): A = value
  }

  trait Fallible[Source, Dest] extends Transformer[Source, Either[String, Dest]]

  object Fallible {
    given identity[A, B >: A]: Transformer.Fallible[A, B] with {
      def transform(value: A): Either[String, B] = Right(value)
    }
  }

  final class Derived[A, B](f: A => B) extends Transformer[A, B] {
    def transform(value: A): B = f(value)
  }
}
