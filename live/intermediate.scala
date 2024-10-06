package live

import java.time.Instant
import scala.deriving.Mirror
import scala.compiletime.*

object intermediate {

  case class Single(
      name: String,
      album: String,
      releaseTimestamp: Long
  )

  case class Track(
      name: String,
      album: String,
      releaseTimestamp: Instant
  )

  extension [Source <: Product](self: Source) {
    inline def convertTo[Dest](using
        Source: Mirror.ProductOf[Source],
        Dest: Mirror.ProductOf[Dest]
    ): Dest = ???
  }

  @main def intermediateTest = {
    val single =
      Single(
        "You Might Think He Loves You for Your Money but I Know What He Really Loves You for It's Your Brand New Leopard-Skin Pill-Box Hat",
        "Government Plates",
        1384300801000L
      )

    val expected =
      Track(
        "You Might Think He Loves You for Your Money but I Know What He Really Loves You for It's Your Brand New Leopard-Skin Pill-Box Hat",
        "Government Plates",
        Instant.ofEpochMilli(1384300801000L)
      )

    // given Transformer[Long, Instant] = Instant.ofEpochMilli
    // given Transformer[Instant, Long] = _.toEpochMilli()

    val actual = single.convertTo[Track]
    val actualReverse = actual.convertTo[Single]

    println(actual)
    println(actualReverse)

    assert(actual == expected)
    assert(single == actualReverse)
  }
}
