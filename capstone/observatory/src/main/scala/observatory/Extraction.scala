package observatory

import java.nio.file.Paths
import java.time.LocalDate

import org.apache.spark.{SparkConf, SparkContext}

/**
  * 1st milestone: data extraction
  */
object Extraction {
  import org.apache.log4j.{Level, Logger}
  Logger.getLogger("org.apache.spark").setLevel(Level.WARN)

  private lazy val conf: SparkConf = new SparkConf().setMaster("local[*]").setAppName("observatory")
  private lazy val sc: SparkContext = new SparkContext(conf)

  private def fsPath(resource: String): String = Paths.get(getClass.getResource(resource).toURI).toString

  /**
    * @param year             Year number
    * @param stationsFile     Path of the stations resource file to use (e.g. "/stations.csv")
    * @param temperaturesFile Path of the temperatures resource file to use (e.g. "/1975.csv")
    * @return A sequence containing triplets (date, location, temperature)
    */
  def locateTemperatures(year: Year, stationsFile: String, temperaturesFile: String): Iterable[(LocalDate, Location, Temperature)] = {
    val stationsRdd = sc.textFile(fsPath(stationsFile))
      .map(Station(_))
      .flatMap(x => x.location.map((x.id, _)))

    val tempRdd = sc.textFile(fsPath(temperaturesFile))
      .map(s => {
        val st = StationTemperature(s, year)
        (st.stationId, (st.date, st.temperature))
      })

    tempRdd.join(stationsRdd)
      .map {
        case (_, ((date, temp), location)) => (date, location, temp)
      }
      .collect
      .toList
      .sortWith((x1, x2) => x1._1.toEpochDay < x2._1.toEpochDay)
  }

  /**
    * @param records A sequence containing triplets (date, location, temperature)
    * @return A sequence containing, for each location, the average temperature over the year.
    */
  def locationYearlyAverageRecords(records: Iterable[(LocalDate, Location, Temperature)]): Iterable[(Location, Temperature)] = {
    sc.parallelize(records.toSeq)
      .map {
        case (_, loc, temp) => (loc, temp)
      }
      .groupByKey
      .mapValues(x => x.sum / x.size)
      .collect().toList
  }
}
