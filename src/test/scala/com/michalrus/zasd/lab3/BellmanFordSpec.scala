package com.michalrus.zasd.lab3

import com.michalrus.zasd.{ MatrixGraph, UnitSpec }

class BellmanFordSpec extends UnitSpec {

  val populate = populateWith("../lab2/graph.txt")_

  "BellmanFord" should {
    "work" in {
      val g = new MatrixGraph[Int]
      timed("populate") { populate(g) }
      timed("run") {
        val result = BellmanFord.arrayTailRec(g, 109)
        info(s"distance(609) = ${result.distance(609)}")
        info(s"predecessor(609) = ${result.predecessor(609)}")
      }
    }
  }

}
