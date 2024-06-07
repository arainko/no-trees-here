import scala.deriving.Mirror
import NamedTuple.*

final class Selector[Names <: Tuple, Types <: Tuple] extends Selectable {
  final type Fields = Field.NamedOf[Names, Types]

  def selectDynamic(name: String): Field[name.type, Nothing] = Field(name)
}

object Selector {
  def of[Names <: Tuple, Types <: Tuple]: Selector[Names, Types] = Selector()
}
