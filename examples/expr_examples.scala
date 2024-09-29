package examples

object expr_examples {
  import scala.quoted.*

  inline def literals[A <: Tuple]: List[String] = ${ literalStringsHighLevel[A] }

  inline def literalsLow[A <: Tuple]: List[String] = ${ literalStringsLowLevel[A] }

  def literalStringsHighLevel[A <: Tuple: Type](using Quotes): Expr[List[String]] = {
    def recurse[T <: Tuple: Type](using Quotes): List[String] = {
      import quotes.reflect.*
      Type.of[T] match {
        case '[EmptyTuple] => Nil
        case '[type head <: String; head *: tail] => 
          val value = Type.valueOfConstant[head]
            .getOrElse(report.errorAndAbort("Not a literal String type"))
          value :: recurse[tail]
      }
    }
    Expr.ofList(recurse[A].map(Expr.apply))
  }

  def literalStringsLowLevel[A <: Tuple: Type](using Quotes): Expr[List[String]] = {
    import quotes.reflect.*
    def recurse(using Quotes)(tpe: quotes.reflect.TypeRepr): List[String] = {
      import quotes.reflect.*
      tpe match {
        case AppliedType(_, ConstantType(StringConstant(value)) :: tail :: Nil) =>
          value :: recurse(tail)
        case _ =>
          Nil
      }
    }
    Expr.ofList(recurse(TypeRepr.of[A]).map(Expr.apply))
  }


}
