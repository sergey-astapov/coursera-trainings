package observatory

import java.io.File

import observatory.Extraction.{locateTemperatures, locationYearlyAverageRecords}
import observatory.Interaction._
import observatory.TemperatureColor.table
import org.scalatest.FunSuite
import org.scalatest.prop.Checkers

trait InteractionTestTrait extends FunSuite with Checkers {
  test("tileLocation should work") {
    assertResult(Location(85.05112877980659, -180.0))(tileLocation(Tile(0, 0, 1)))
  }

  test("tile should work") {
    val iter = locateTemperatures(2015,
      "/test-stations.csv",
      "/test-2015.csv")
    val tuples = locationYearlyAverageRecords(iter)
    val image = tile(tuples, table, Tile(0, 0, 1))
    assertResult(256)(image.width)
    assertResult(256)(image.height)
    image.output(new File("test-tile-out.png"))
  }

  test("zoom2tiles should work") {
    assertResult(List(Tile(0, 0, 0)))(zoom2tiles(0).toList)
    assertResult(List(Tile(0,0,1), Tile(0,1,1), Tile(1,0,1), Tile(1,1,1)))(zoom2tiles(1).toList)
  }

  test("generateTiles should work") {
    generateTiles[Int](List((1975, 1975), (1976, 1976)),
      (y, t, d) => println(s"${t.toPath(y)} data: $d")
    )
  }
}

class InteractionTest extends InteractionTestTrait
