package observatory

import java.time.LocalDate

import com.sksamuel.scrimage.RGBColor

/**
  * Introduced in Week 1. Represents a location on the globe.
  *
  * @param lat Degrees of latitude, -90 ≤ lat ≤ 90
  * @param lon Degrees of longitude, -180 ≤ lon ≤ 180
  */
case class Location(lat: Double, lon: Double) {
  def antipodes: List[Location] = List(Location(-lat, lon - 180), Location(-lat, lon + 180))

  def haversineEarthDistance(other: Location): Double =
    LocationRadians(this) haversineEarthDistance LocationRadians(other)
}

import java.lang.Math._

case class LocationRadians(loc: Location) {
  val earthRadius = 6372.8
  val latRadians: Double = toRadians(loc.lat)
  val lonRadians: Double = toRadians(loc.lon)

  def haversineEarthDistance(other: LocationRadians): Double = {
    earthRadius * greatCircleDistance(other) * 1000
  }

  def greatCircleDistance(other: LocationRadians): Double = {
    val deltaLat = abs(other.latRadians - latRadians)
    val deltaLon = abs(other.lonRadians - lonRadians)

    val a =  pow(sin(deltaLat / 2), 2) + cos(latRadians) * cos(other.latRadians) * pow(sin(deltaLon / 2), 2)
    2 * atan2(sqrt(a), sqrt(1 - a))
  }

}

/**
  * Introduced in Week 3. Represents a tiled web map tile.
  * See https://en.wikipedia.org/wiki/Tiled_web_map
  * Based on http://wiki.openstreetmap.org/wiki/Slippy_map_tilenames
  * @param x X coordinate of the tile
  * @param y Y coordinate of the tile
  * @param zoom Zoom level, 0 ≤ zoom ≤ 19
  */
case class Tile(x: Int, y: Int, zoom: Int) {
  def toLatLon = LatLonPoint(
    toDegrees(atan(sinh(PI * (1.0 - 2.0 * y.toDouble / (1 << zoom))))),
    x.toDouble / (1 << zoom) * 360.0 - 180.0,
    zoom)

  def toLocation: Location = toLatLon.toLocation

  def toURI = new java.net.URI(s"https://tile.openstreetmap.org/$zoom/$x/$y.png")

  def toPath: Year => String = year => s"target/temperatures/$year/$zoom/$x-$y.png"
}

case class LatLonPoint(lat: Double, lon: Double, zoom: Int) {
  def toLocation: Location = Location(lat, lon)

  def toTile = Tile(
    ((lon + 180.0) / 360.0 * (1 << zoom)).toInt,
    ((1 - log(tan(toRadians(lat)) + 1 / cos(toRadians(lat))) / PI) / 2.0 * (1 << zoom)).toInt,
    zoom)
}

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
case class Color(red: Int, green: Int, blue: Int) {
  def pixel(alpha: Int = 255) = RGBColor(red, green, blue, alpha).toPixel
}

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

case class TemperatureColor(temperature: Temperature, color: Color)

object TemperatureColor {

  val table = List(
    (-60.0, Color(0, 0, 0)),
    (-50.0, Color(33, 0, 107)),
    (-27.0, Color(255, 0, 255)),
    (-15.0, Color(0, 0, 255)),
    (0.0, Color(0, 255, 255)),
    (12.0, Color(255, 255, 0)),
    (32.0, Color(255, 0, 0)),
    (60.0, Color(255, 255, 255))
  )

  private def interpolate(temp: Temperature, tempMin: Temperature, tempMax: Temperature): (Int, Int) => Int = {
    case (colMin, colMax) =>
      val factor = (temp - tempMin) / (tempMax - tempMin)
      Math.round(colMin + (colMax - colMin) * factor).toInt
  }

  def interpolate(temp: Temperature, min: (Temperature, Color), max: (Temperature, Color)): Color = {
    val func = interpolate(temp, min._1, max._1)
    Color(
      func(min._2.red, max._2.red),
      func(min._2.green, max._2.green),
      func(min._2.blue, max._2.blue)
    )
  }

  def interpolate(points: Iterable[(Temperature, Color)], value: Temperature): Color = {
    val arr = points.toArray
    points.zipWithIndex.find {
      case ((temp, _), _) => value <= temp
    }.map {
      case ((_, _), i) => i
    } match {
      case Some(0) => table.head._2
      case Some(i) => interpolate(value, arr(i - 1), arr(i))
      case None => table.last._2
    }
  }

  def interpolate(value: Temperature): Color = interpolate(table, value)
}

