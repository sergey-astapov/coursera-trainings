package observatory

import java.io.File

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.TemperatureColor.table
import observatory.Visualization._
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTestTrait extends FunSuite with Checkers {
  test("interpolateColor should work") {
    assertResult(Color(0, 0, 0))(interpolateColor(table, -60.0))
    assertResult(Color(255, 255, 255))(interpolateColor(table, 60.0))
    assertResult(Color(255,153,0))(interpolateColor(table, 20.0))
  }

  test("pixel location should work") {
    assertResult(Location(90, -180))(pixelLocation(360, 180, 0))
    assertResult(Location(90, 179))(pixelLocation(360, 180, 359))
    assertResult(Location(0, 0))(pixelLocation(360, 180, 360 * 90 + 180))
    assertResult(Location(-89, 179))(pixelLocation(360, 180, 360 * 180 - 1))
    assertResult(Location(-89, -180))(pixelLocation(360, 180, 360 * 179))
  }

  test("visualization shoud work") {
    val iter = locateTemperatures(2015,
      "/test-stations.csv",
      "/test-2015.csv")
    val tuples = locationYearlyAverageRecords(iter)
    val image = visualize(tuples, table)
    assertResult(360)(image.width)
    assertResult(180)(image.height)
    image.output(new File("test-out.png"))
  }
}

class VisualizationTest extends VisualizationTestTrait {}
