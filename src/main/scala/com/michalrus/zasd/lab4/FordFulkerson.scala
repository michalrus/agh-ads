package com.michalrus.zasd.lab4

import com.michalrus.zasd.Graph

object FordFulkerson {

  trait Result[EdgeWeight] {
    def flow: EdgeWeight
  }

  def arrayTailRec(g: Graph[Int, Int], source: Int, sink: Int): Result[Int] = {
    ???
  }

}
