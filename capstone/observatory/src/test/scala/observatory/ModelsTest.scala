package observatory

import java.time.LocalDate

import observatory.StationTemperature.far2cel
import observatory.TemperatureColor.interpolate
import org.scalatest.FunSuite

trait ModelsTestTrait extends StationTest
  with StationTemperatureTest
  with TempreratureColorTest
  with TileTestTrait

class ModelsTest extends ModelsTestTrait {}

trait StationTest extends FunSuite {
  test("parse '007005,,,' should work") {
    val sut = Station("007005,,,")
    assertResult(StationId("007005", ""))(sut.id)
    assertResult(None)(sut.location)
  }

  test("parse '008268,03707,+32.950,+065.567' should work") {
    val sut = Station("008268,03707,+32.950,-065.567")
    assertResult(StationId("008268", "03707"))(sut.id)
    assertResult(Some(Location(32.950, -65.567)))(sut.location)
  }

  test("parse ',03707,+32.950,+065.567' should work") {
    val sut = Station(",03707,+32.950,-065.567")
    assertResult(StationId("", "03707"))(sut.id)
    assertResult(Some(Location(32.950, -65.567)))(sut.location)
  }
}

trait StationTemperatureTest extends FunSuite {
  test("parse '010013,,11,25,39.2' should work") {
    val sut = StationTemperature("010013,,11,25,39.2", 2015)
    assertResult(StationId("010013", ""))(sut.stationId)
    assertResult(LocalDate.of(2015, 11, 25))(sut.date)
    assertResult(far2cel(39.2))(sut.temperature)
  }

  test("parse '010013,03707,11,25,39.2' should work") {
    val sut = StationTemperature("010013,03707,11,25,39.2", StationTemperature.fn2year("/2015.csv"))
    assertResult(StationId("010013", "03707"))(sut.stationId)
    assertResult(LocalDate.of(2015, 11, 25))(sut.date)
    assertResult(far2cel(39.2))(sut.temperature)
  }

  test("far2cel should work") {
    assertResult(10.0)(far2cel(50.0))
  }
}

trait TempreratureColorTest extends FunSuite {
  test("interpolate should work") {
    val sut = interpolate(-55.0, (-60.0, Color(0, 0, 0)), (-50.0, Color(33, 0, 107)))
    assertResult(Color(17, 0, 54))(sut)
  }
}

trait TileTestTrait extends FunSuite {
  test("tile to path should work") {
    assertResult("target/temperatures/2015/0/0-0.png")(Tile(0, 0, 0).toPath(2015))
  }
}

class TileTest extends TileTestTrait