package com.michalrus.zasd.lab2

import com.michalrus.zasd._
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.{ Seconds, Span }

import scala.io.Source

class WarshallFloydSpec extends UnitSpec with Timeouts {

  def populate(g: Graph[Int, Int]): Unit =
    GraphParser.addFromString(g, Source.fromURL(getClass.getResource("graph.txt")).mkString)

  def timed[F](label: String)(b: ⇒ F): F = {
    val start = System.nanoTime
    val f = b
    val stop = System.nanoTime
    info(s"t_$label = ${(stop - start) / 1e9} s")
    f
  }

  "GraphParser" should {
    "succeed in populating a MatrixGraph" in timed("populate") { populate(new MatrixGraph[Int]) }
    "succeed in populating an AdjacencyGraph" in timed("populate") { populate(new AdjacencyGraph[Int, Int]) }
  }

  def variant(v1: Int, v2: Int, vname: String, v: (WarshallFloyd.type, Graph[Int, Int]) ⇒ WarshallFloyd.Result[Int], tmout: Long, ignored: Boolean): Unit = {
    def forGraph(gname: String, gen: ⇒ Graph[Int, Int]) {
      lazy val body: Unit = {
        val g = gen
        timed("populate") { populate(g) }
        val result = failAfter(Span(tmout, Seconds)) {
          timed("run") { v(WarshallFloyd, g) }
        }
        info(s"distance($v1, $v2) = ${result.distance(v1, v2)}")
        info(s"shortestPath($v1, $v2) = ${result.shortestPath(v1, v2)}")
      }
      val msg = s"take less than $tmout s for $gname"
      if (ignored) msg ignore body else msg in body
    }

    s"using `$vname` variant" should {
      forGraph("a MatrixGraph", new MatrixGraph[Int])
      forGraph("an AdjacencyGraph", new AdjacencyGraph[Int, Int])
    }
  }

  "WarshallFloyd" when {
    variant(109, 609, "mutableMap", _ mutableMap _, 600, ignored = true)
    variant(109, 609, "mutableArray", _ mutableArray _, 30, ignored = true)
    variant(109, 609, "rawArray", _ rawArray _, 10, ignored = false)
  }

}
