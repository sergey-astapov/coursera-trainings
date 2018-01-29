package observatory

import java.lang.Math._

import com.sksamuel.scrimage.Image
import observatory.Visualization.{interpolateColor, pixelLocation, predictTemperature}

/**
  * 2nd milestone: basic visualization
  */
object Visualization {
  def distance(first: Location, second: Location): Double = {
    val delta = (first, second) match {
      case (Location(lat1, lon1), Location(lat2, lon2)) =>
        val latDist = toRadians(lat2 - lat1)
        val lonDist = toRadians(lon2 - lon1)
        val a = pow(sin(latDist / 2), 2) + cos(toRadians(lat1)) * cos(toRadians(lat2)) * pow(sin(lonDist / 2), 2)

        2 * atan2(sqrt(a), sqrt(1 - a))
    }
    delta * 6371
  }

  /**
    * @param temperatures Known temperatures: pairs containing a location and the temperature at this location
    * @param location Location where to predict the temperature
    * @return The predicted temperature at `location`
    */
  def predictTemperature(temperatures: Iterable[(Location, Temperature)], location: Location): Temperature = {
    val predictions: Iterable[(Double, Double)] = temperatures.map {
      case (other, temp) => (location.haversineEarthDistance(other), temp)
    }

    predictions.find(_._1 == 0.0) match {
      case Some((_, temp)) => temp
      case _ => inverseDistanceWeighted(predictions, power = 3)
    }
  }

  def inverseDistanceWeighted(predictions: Iterable[(Double, Double)], power: Int): Double = {
    val (weightedSum, inverseWeightedSum) = predictions.aggregate((0.0, 0.0))(
        {
          case ((ws, iws), (distance, temp)) =>
            val w = 1 / pow(distance, power)
            (w * temp + ws, w + iws)
        }, {
          case ((wsA, iwsA), (wsB, iwsB)) => (wsA + wsB, iwsA + iwsB)
        }
      )

    weightedSum / inverseWeightedSum
  }


  /**
    * @param points Pairs containing a value and its associated color
    * @param value The value to interpolate
    * @return The color that corresponds to `value`, according to the color scale defined by `points`
    */
  def interpolateColor(points: Iterable[(Temperature, Color)], value: Temperature): Color =
    TemperatureColor.interpolate(points, value)

  /**
    * @param temperatures Known temperatures
    * @param colors Color scale
    * @return A 360×180 image where each pixel shows the predicted temperature at its location
    */
  def visualize(temperatures: Iterable[(Location, Temperature)], colors: Iterable[(Temperature, Color)]): Image = {
    ImageBuilder(temperatures, colors, 360, 180, 255).build
  }

  def pixelLocation(w: Int, h: Int, i: Int): Location = {
    val x: Int = i % w
    val y: Int = i / w

    Location(90 - y, x - 180)
  }
}

class ImageBuilder(temperatures: Iterable[(Location, Temperature)],
                   colors: Iterable[(Temperature, Color)],
                   width: Int,
                   height: Int,
                   alpha: Int) {
  def build: Image = {
    val pixels = (0 until width * height).par
      .map(i => interpolateColor(
        colors,
        predictTemperature(
          temperatures,
          pixelLocation(width, height, i)
        )
      ).pixel(alpha))
      .toArray
    Image(width, height, pixels)
  }
}

object ImageBuilder {
  def apply(temperatures: Iterable[(Location, Temperature)],
            colors: Iterable[(Temperature, Color)],
            width: Int,
            height: Int,
            alpha: Int): ImageBuilder = new ImageBuilder(temperatures, colors, width, height, alpha)
}

class TileImageBuilder(temperatures: Iterable[(Location, Temperature)],
                       colors: Iterable[(Temperature, Color)],
                       tile: Tile,
                       width: Int,
                       height: Int,
                       alpha: Int) {
  def build: Image = {
    val pixels = (0 until width * height).par
      .map { i =>
        val x = (i % width).toDouble / width + tile.x
        val y = (i / height).toDouble / height + tile.y
        interpolateColor(
          colors,
          predictTemperature(
            temperatures,
            Tile(x.toInt, y.toInt, tile.zoom).toLocation
          )
        ).pixel(alpha)
      }
      .toArray
    Image(width, height, pixels)
  }
}

object TileImageBuilder {
  def apply(temperatures: Iterable[(Location, Temperature)],
            colors: Iterable[(Temperature, Color)],
            tile: Tile,
            width: Int,
            height: Int,
            alpha: Int = 127): TileImageBuilder = new TileImageBuilder(temperatures, colors, tile, width, height, alpha)
}

