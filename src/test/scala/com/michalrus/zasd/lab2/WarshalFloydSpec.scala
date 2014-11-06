package com.michalrus.zasd.lab2

import com.michalrus.zasd._
import org.scalatest.concurrent.Timeouts
import org.scalatest.time.{ Seconds, Span }

import scala.io.Source

class WarshalFloydSpec extends UnitSpec with Timeouts {

  def populate(g: Graph[Int, Int]): Unit =
    GraphParser.addFromString(g, Source.fromURL(getClass.getResource("graph.txt")).mkString)

  "GraphParser" should {
    "succeed in populating a MatrixGraph" in { populate(new MatrixGraph[Int]) }
    "succeed in populating an AdjacencyGraph" in { populate(new AdjacencyGraph[Int, Int]) }
  }

  def variant[T](vname: String, v: (WarshalFloyd.type, Graph[Int, Int]) ⇒ T, tmout: Long, ignored: Boolean): Unit = {
    def forGraph(gname: String, gen: ⇒ Graph[Int, Int]) {
      lazy val body: Unit = {
        val g = gen
        populate(g)
        val start = System.nanoTime
        failAfter(Span(tmout, Seconds)) {
          v(WarshalFloyd, g)
        }
        val stop = System.nanoTime
        info(s"time = ${(stop - start) / 1e9} s")
      }
      val msg = s"take less than $tmout s for $gname"
      if (ignored) msg ignore body else msg in body
    }

    s"using `$vname` variant" should {
      forGraph("a MatrixGraph", new MatrixGraph[Int])
      forGraph("an AdjacencyGraph", new AdjacencyGraph[Int, Int])
    }
  }

  "WarshalFloyd" when {
    variant("mutableMap", _ mutableMap _, 600, ignored = true)
    variant("mutableArray", _ mutableArray _, 30, ignored = false)
  }

}
