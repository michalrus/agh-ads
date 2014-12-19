package com.michalrus.zasd.lab3

import com.michalrus.zasd.Graph

object BellmanFord {

  trait Result[Vertex, EdgeWeight] {
    def distance(v: Vertex): EdgeWeight
    def predecessor(v: Vertex): Vertex
  }

  def arrayTailRec(g: Graph[Int, Int], source: Int): Result[Int, Int] = {
    val vs = g.vertices
    val size = vs.size
    val N = vs.max
    val d = Array.ofDim[Int](N + 1)
    val predec = Array.ofDim[Int](N + 1)

    import annotation.tailrec

    @tailrec def init(v: Int): Unit = {
      if (v == source) d(v) = 0
      else d(v) = Int.MaxValue
      predec(v) = -1
      if (v < N) init(v + 1)
    }
    init(0)

    @tailrec def loop1(i: Int): Unit = {
      @tailrec def loop2(u: Int): Unit = {
        val edges = g.edgesOutOf(u).toVector
        val nEdges = edges.size
        @tailrec def loop3(j: Int): Unit = {
          val v = edges(j)._1
          val w = edges(j)._2
          val sum = d(u).toLong + w.toLong
          if (sum < d(v)) {
            d(v) = sum.toInt
            predec(v) = u
          }
          if (j < nEdges - 1) loop3(j + 1)
        }
        loop3(0)
        if (u < N) loop2(u + 1)
      }
      loop2(0)
      if (i < size - 1) loop1(i + 1)
    }
    loop1(1) // Why 1?

    // TODO: check for negative weight cycles
    // for each edge (u, v) with weight w in edges:
    // if weight[u] + w < weight[v]:
    //   error "Graph contains a negative-weight cycle"

    new Result[Int, Int] {
      def distance(v: Int): Int = d(v)
      def predecessor(v: Int): Int = predec(v)
    }
  }

}
