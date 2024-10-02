package done

import NamedTuple.*

enum Config[-Source, +FieldTpe] {
  case Const(value: FieldTpe)
  case Computed(function: Source => FieldTpe)
}
