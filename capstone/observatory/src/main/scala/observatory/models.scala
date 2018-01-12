package observatory

import java.time.LocalDate

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double)

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int)

/**
  * Introduced in Week 4. Represents a point on a grid composed of
  * circles of latitudes and lines of longitude.
  * @param lat Circle of latitude in degrees, -89 ≤ lat ≤ 90
  * @param lon Line of longitude in degrees, -180 ≤ lon ≤ 179
  */
case class GridLocation(lat: Int, lon: Int)

/**
  * Introduced in Week 5. Represents a point inside of a grid cell.
  * @param x X coordinate inside the cell, 0 ≤ x ≤ 1
  * @param y Y coordinate inside the cell, 0 ≤ y ≤ 1
  */
case class CellPoint(x: Double, y: Double)

/**
  * Introduced in Week 2. Represents an RGB color.
  * @param red Level of red, 0 ≤ red ≤ 255
  * @param green Level of green, 0 ≤ green ≤ 255
  * @param blue Level of blue, 0 ≤ blue ≤ 255
  */
case class Color(red: Int, green: Int, blue: Int)

case class StationId(stn: String, wban: String)

object StationId {
  def apply(list: List[String]): StationId = list match {
    case stn :: wban :: _ => StationId(stn, wban)
    case stn :: Nil => StationId(stn, "")
    case Nil => StationId("", "")
  }
}

case class Station(id: StationId, location: Option[Location])

object Station {
  def apply(s: String): Station = {
    val list = s.split(",").toList
    val id = StationId(list)
    list match {
      case _ :: _ :: lat :: lon :: Nil => Station(id, Some(Location(lat.toDouble, lon.toDouble)))
      case _ => Station(id, None)
    }
  }
}

case class StationTemperature(stationId: StationId, date: LocalDate, temperature: Temperature)

object StationTemperature {
  def apply(s: String, year: Year): StationTemperature = {
    val list = s.split(",").toList
    val id = StationId(list)
    list match {
      case _ :: _ :: month :: day :: t :: Nil =>
        StationTemperature(id, LocalDate.of(year, month.toInt, day.toInt), far2cel(t.toDouble))
      case _ => throw new IllegalArgumentException(s"$s is wrong")
    }
  }

  def far2cel: Double => Double = x => (x - 32.0) * 5 / 9

  def fn2year: String => Year = x => x.replace("/", "").replace(".csv", "").toInt
}

