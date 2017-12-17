package stackoverflow

import org.scalatest.{BeforeAndAfterAll, FunSuite}
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.SparkContext._
import org.apache.spark.rdd.RDD
import java.io.File

import stackoverflow.StackOverflow.postingFunc

@RunWith(classOf[JUnitRunner])
class StackOverflowSuite extends FunSuite with BeforeAndAfterAll {


  lazy val testObject = new StackOverflow {
    override val langs =
      List(
        "JavaScript", "Java", "PHP", "Python", "C#", "C++", "Ruby", "CSS",
        "Objective-C", "Perl", "Scala", "Haskell", "MATLAB", "Clojure", "Groovy")

    override def langSpread = 50000

    override def kmeansKernels = 45

    override def kmeansEta: Double = 20.0D

    override def kmeansMaxIterations = 120
  }

  test("testObject can be instantiated") {
    val instantiatable = try {
      testObject
      true
    } catch {
      case _: Throwable => false
    }
    assert(instantiatable, "Can't instantiate a StackOverflow object")
  }
}

class SomeTests extends FunSuite {
  lazy val conf: SparkConf = new SparkConf().setMaster("local").setAppName("Tests")
  lazy val sc: SparkContext = new SparkContext(conf)
  private val postData ="""
                  |1,1,,,0,C#
                  |2,2,,1,5,C#
                  |1,3,,,0,Java
                  |2,4,,3,3,Java
                  |2,5,,1,9,C#
                  |2,6,,3,10,Java
                  |""".stripMargin

  test("groupedPostings test") {
    val seq = postData.split("\n").toSeq
      .filter(_.nonEmpty)
      .map(postingFunc)
    val rdd = sc.parallelize(seq)
    val grouped = StackOverflow.groupedPostings(rdd)
    grouped.collect().foreach(println)

    val scored = StackOverflow.scoredPostings(grouped)
    scored.collect().foreach(println)

    val vector = StackOverflow.vectorPostings(scored)
    vector.collect().foreach(println)
  }
}
