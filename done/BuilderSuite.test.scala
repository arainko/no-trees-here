package done

import munit.FunSuite

class BuilderSuite extends FunSuite {
  test("") {
    case class Costam(int: Int, str: String)
    case class Costam2(int: Int, str2: String)

    val res =
      Costam(1, "asd")
        .into[Costam2]
        .withField(_.int)(Config.Computed(a => a.int + 20))
        .withField(_.str2)(Config.Const("asdddd"))
        .transform

    println(res)
  }
}
