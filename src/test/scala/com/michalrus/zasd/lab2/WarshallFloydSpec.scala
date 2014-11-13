package com.michalrus.zasd.lab2

import com.michalrus.zasd._
import org.scalatest.concurrent.{ Interruptor, Timeouts }
import org.scalatest.time.{ Seconds, Span }

import scala.io.Source

class WarshallFloydSpec extends UnitSpec with Timeouts {

  def populate(g: Graph[Int, Int]): Unit =
    GraphParser.addFromString(g, Source.fromURL(getClass.getResource("graph.txt")).mkString)

  def timedWithTime[F](label: String, showInfo: Boolean)(b: ⇒ F): (F, Long) = {
    val start = System.nanoTime
    val f = b
    val stop = System.nanoTime
    val time = stop - start
    if (showInfo) info(s"t_$label = ${time / 1e9} s")
    (f, time)
  }

  def timed[F](label: String)(b: ⇒ F): F = timedWithTime(label, showInfo = true)(b)._1

  "GraphParser" should {
    "succeed in populating a MatrixGraph" in timed("populate") { populate(new MatrixGraph[Int]) }
    "succeed in populating an AdjacencyGraph" in timed("populate") { populate(new AdjacencyGraph[Int, Int]) }
  }

  def variant(numRuns: Int, v1: Int, v2: Int, expDistance: Int, expPath: List[Int], vname: String, v: (WarshallFloyd.type, Graph[Int, Int]) ⇒ WarshallFloyd.Result[Int], tmout: Long, ignored: Boolean): Unit = {
    def forGraph(gname: String, gen: ⇒ Graph[Int, Int]) {
      lazy val body: Unit = {
        val g = gen
        timed("populate") { populate(g) }

        val runs = (1 to numRuns) map { _ ⇒
          val (result, tme) = failAfter(Span(tmout, Seconds)) {
            timedWithTime("run", showInfo = false) { v(WarshallFloyd, g) }
          }
          (tme, result.distance(v1, v2), result.shortestPath(v1, v2))
        }

        val times = runs map (_._1.toDouble / 1e9)
        // info(s"t_run_avg = ${times.sum / times.size}")
        info(s"t_run_min = ${times.min}")

        info(s"distance($v1, $v2) = ${runs.head._2}")
        info(s"shortestPath($v1, $v2) = ${runs.head._3}")

        runs.head._2 shouldBe expDistance
        runs.head._3 shouldBe expPath
      }
      val msg = s"take less than $tmout s for $gname and be correct"
      if (ignored) msg ignore body else msg in body
    }

    s"using `$vname` variant (run $numRuns×)" should {
      forGraph("a MatrixGraph", new MatrixGraph[Int])
      forGraph("an AdjacencyGraph", new AdjacencyGraph[Int, Int])
    }
  }

  "WarshallFloyd" when {
    val r = 1
    val v1 = 109
    val v2 = 609
    val d = 18
    val p = List(109, 713, 870, 614, 808, 609)
    variant(r, v1, v2, d, p, "mutableMap", _ mutableMap _, 600, ignored = true)
    variant(r, v1, v2, d, p, "mutableArray", _ mutableArray _, 30, ignored = true)
    variant(r, v1, v2, d, p, "rawArray", _ rawArray _, 10, ignored = false)
    variant(r, v1, v2, d, p, "rawRawArray", _ rawRawArray _, 10, ignored = false)
  }

  "WarshallFloyd" should {
    "report correct R = t_Adjacency / t_Matrix" in {
      val ga = new AdjacencyGraph[Int, Int]
      val gm = new MatrixGraph[Int]
      timed("populate") {
        populate(ga)
        populate(gm)
      }
      val (_, ta) = timedWithTime("Adjacency", showInfo = true) { WarshallFloyd.rawArray(ga) }
      val (_, tm) = timedWithTime("Matrix", showInfo = true) { WarshallFloyd.rawArray(gm) }
      val R = ta.toDouble / tm.toDouble
      info(s"R = $R")
      R should be < 1.5
      R should be > 0.5
    }
  }

}
