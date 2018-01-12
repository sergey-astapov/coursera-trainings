package observatory

import observatory.Extraction._
import org.scalatest.FunSuite

trait ExtractionTest extends FunSuite {
  test("Extraction should work") {
    val iter = locateTemperatures(2015, "/test-stations.csv", "/test-2015.csv")
    assert(iter.nonEmpty)
    iter.take(20).foreach(println)

    val tuples = locationYearlyAverageRecords(iter)
    assert(tuples.nonEmpty)
    tuples.take(5).foreach(println)
  }
}