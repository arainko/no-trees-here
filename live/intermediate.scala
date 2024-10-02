package live

import java.time.Instant

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

  extension [Source](self: Source) {
    inline def convertTo[Dest]: Dest = ???
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

    val actual = single.convertTo[Track]
    val actualReverse = actual.convertTo[Single]

    assert(actual == expected)
    assert(single == actualReverse)
  }
}
