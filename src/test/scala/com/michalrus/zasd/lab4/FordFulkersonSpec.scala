package com.michalrus.zasd.lab4

import com.michalrus.zasd.{ MatrixGraph, UnitSpec }

class FordFulkersonSpec extends UnitSpec {

  val populate = populateWith("../lab2/graph.txt")_

  "FordFulkerson" should {
    "work" in {
      val g = new MatrixGraph[Int]
      timed("populate") { populate(g) }
      timed("run") {
        val result = FordFulkerson.arrayTailRec(g, 109, 609)
        info(s"flow(109, 609) = ${result.flow}")
      }
    }
  }

}
