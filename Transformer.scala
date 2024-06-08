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

  // trait Fallible[Dest] extends TransformableInto[]


  // inline def derived[A: Mirror.Of, B: Mirror.Of]: A has TransformableInto[B] = {
  //   def transform(value: A): B = {
  //     val summoned = 
  //   }

  //   Derived[A, B](???)
  // }

  final class Derived[A, B](f: A => B) extends (A has Transformer[B]) {
    def transform(value: Self): B = f(value)
  }
}
