package com.michalrus.zasd.lab2

import com.michalrus.zasd._
import org.scalatest.time.{ Seconds, Span }

import scala.io.Source

class WarshallFloydSpec extends UnitSpec {

  val populate = populateWith("graph.txt")_

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
          val d = result.distance(v1, v2)
          val p = result.shortestPath(v1, v2)
          d shouldBe expDistance
          p shouldBe expPath
          (tme, d, p)
        }

        val times = runs map (_._1.toDouble / 1e9)
        // info(s"t_run_avg = ${times.sum / times.size}")
        info(s"t_run_min = ${times.min} s")

        info(s"distance($v1, $v2) = ${runs.head._2}")
        info(s"shortestPath($v1, $v2) = ${runs.head._3}")
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
    variant(r, v1, v2, d, p, "mutableHashMap", _ mutableHashMap _, 600, ignored = true)
    variant(r, v1, v2, d, p, "arrayForComprehension", _ arrayForComprehension _, 30, ignored = true)
    variant(r, v1, v2, d, p, "arrayTailRec", _ arrayTailRec _, 10, ignored = false)
    variant(r, v1, v2, d, p, "array1DTailRec", _ array1DTailRec _, 10, ignored = false)
  }

  "WarshallFloyd" should {
    "report correct R = t_Adjacency / t_Matrix" in {
      val ga = new AdjacencyGraph[Int, Int]
      val gm = new MatrixGraph[Int]
      timed("populate") {
        populate(ga)
        populate(gm)
      }
      val (_, ta) = timedWithTime("Adjacency", showInfo = true) { WarshallFloyd.arrayTailRec(ga) }
      val (_, tm) = timedWithTime("Matrix", showInfo = true) { WarshallFloyd.arrayTailRec(gm) }
      val R = ta.toDouble / tm.toDouble
      info(s"R = $R")
      R should be < 1.5
      R should be > 0.5
    }
  }

}
