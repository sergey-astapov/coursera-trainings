package observatory

import java.time.LocalDate

import observatory.StationTemperature.far2cel
import org.scalatest.FunSuite

trait ModelsTest extends StationTest with StationTemperatureTest {}

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