import scala.language.experimental.modularity
import scala.deriving.Mirror
import scala.compiletime.*

trait TransformableInto[Dest] {
  type Self

  def transform(value: Self): Dest
}

object TransformableInto {
  given identity[A]: (A is TransformableInto[A]) with {
    def transform(value: Self): A = value
  }


  // inline def derived[A: Mirror.Of, B: Mirror.Of]: A is TransformableInto[B] = {
  //   def transform(value: A): B = {
  //     val summoned = 
  //   }

  //   Derived[A, B](???)
  // }

  final class Derived[A, B](f: A => B) extends (A is TransformableInto[B]) {
    def transform(value: Self): B = f(value)
  }
}
