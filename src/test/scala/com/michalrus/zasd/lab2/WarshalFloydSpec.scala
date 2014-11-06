package com.michalrus.zasd.lab2

import com.michalrus.zasd._
import org.scalatest.concurrent.Timeouts
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.time.{ Seconds, Span }

import scala.io.Source

class WarshalFloydSpec extends UnitSpec with Timeouts {

  def populate(g: Graph[Int, Int]): Unit =
    GraphParser.addFromString(g, Source.fromURL(getClass.getResource("graph.txt")).mkString)

  "GraphParser" should {
    "succeed in populating a MatrixGraph" in { populate(new MatrixGraph[Int]) }
    "succeed in populating an AdjacencyGraph" in { populate(new AdjacencyGraph[Int, Int]) }
  }

  def timeout[T](tmout: Long, v: (WarshalFloyd.type, Graph[Int, Int]) ⇒ T, g: String, gen: ⇒ Graph[Int, Int]) = {
    s"take more than $tmout seconds on a(n) $g" ignore {
      val _ = intercept[TestFailedDueToTimeoutException] {
        val g = gen
        populate(g)
        failAfter(Span(tmout, Seconds)) {
          v(WarshalFloyd, g)
        }
      }
    }
  }

  "WarshalFloyd" when {
    "using `mutableMap` variant" should {
      timeout(60, _ mutableMap _, "MatrixGraph", new MatrixGraph[Int])
      timeout(60, _ mutableMap _, "AdjacencyGraph", new AdjacencyGraph[Int, Int])
    }
  }

}
