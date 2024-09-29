package examples

@main def main = {
  println(expr_examples.literals["one" *: "two" *: "three" *: EmptyTuple])
  println(expr_examples.literalsLow["one" *: "two" *: "three" *: EmptyTuple])
}
