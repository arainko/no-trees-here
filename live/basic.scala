package live

import java.time.LocalDate

object basic {
  case class Album(
      name: String,
      artist: String,
      releaseDate: LocalDate
  )

  case class EverSoSlightlyMoreDetailedAlbum(
      releaseDate: LocalDate,
      artist: String,
      name: String,
      numberOfTracks: Int
  )

  extension [Source](self: Source) {
    inline def convertTo[Dest]: Dest = ???
  }

  @main def basicTest = {
    val detailedAlbum =
      EverSoSlightlyMoreDetailedAlbum(
        releaseDate = LocalDate.of(2024, 9, 27),
        artist = "Xiu Xiu",
        name = "13'' Frank Beltrame Italian Stiletto with Bison Horn Grips",
        numberOfTracks = 9
      )

    val expected = Album(
      name = "13'' Frank Beltrame Italian Stiletto with Bison Horn Grips",
      artist = "Xiu Xiu",
      releaseDate = LocalDate.of(2024, 9, 27)
    )

    val actual = detailedAlbum.convertTo[Album]

    assert(actual == expected)
  }
}
