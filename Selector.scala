import scala.deriving.Mirror
import Transformable.*
import NamedTuple.*

final class Selector[A] extends Selectable {
  final type Fields = Field.Of[A]

  def selectDynamic(name: String): Field[name.type, Nothing] = Field(name)
}

object Selector {
  def of[A]: Selector[A] = Selector[A] 
}