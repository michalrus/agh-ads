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

  "WarshalFloyd" when {
    "using `mutableMap` variant" should {
      "take more than 120 seconds on MatrixGraph" in intercept[TestFailedDueToTimeoutException] {
        val g = new MatrixGraph[Int]
        populate(g)
        failAfter(Span(120, Seconds)) {
          WarshalFloyd.mutableMap(g)
        }
      }
    }
  }

}
