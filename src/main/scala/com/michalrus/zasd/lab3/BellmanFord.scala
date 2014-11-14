package com.michalrus.zasd.lab3

import com.michalrus.zasd.Graph

object BellmanFord {

  trait Result[Vertex, EdgeWeight] {
    def distance(v: Vertex): EdgeWeight
    def predecessor(v: Vertex): Vertex
  }

  def arrayTailRec(g: Graph[Int, Int], source: Int): Result[Int, Int] = {
    ???
  }

}
