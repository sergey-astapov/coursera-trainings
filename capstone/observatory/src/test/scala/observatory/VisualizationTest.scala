package observatory


import observatory.TemperatureColor.table
import observatory.Visualization.interpolateColor
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait VisualizationTestTrait extends FunSuite with Checkers {
  test("interpolateColor should work") {
    assertResult(Color(0, 0, 0))(interpolateColor(table, -60.0))
    assertResult(Color(255, 255, 255))(interpolateColor(table, 60.0))
    assertResult(Color(255,153,0))(interpolateColor(table, 20.0))
  }
}

class VisualizationTest extends VisualizationTestTrait {}
