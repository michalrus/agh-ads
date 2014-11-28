package com.michalrus.zasd.lab4

import com.michalrus.zasd.Graph

object FordFulkerson {

  trait Result[EdgeWeight] {
    def flow: EdgeWeight
  }

  def arrayTailRec(g: Graph[Int, Int], source: Int, sink: Int): Result[Int] = {
    val N = g.vertices.max + 1
    val f = Array.ofDim[Int](N, N)

    case class Edge(u: Int, v: Int, w: Int)

    def findPath: Vector[Edge] = {
      def go(from: Int, to: Int, acc: Vector[Edge]): Vector[Edge] = {
        if (from == to) acc
        else {
          var rv = Vector.empty[Edge]
          for {
            edge ← g.edgesOutOf(from) map { case (v, w) ⇒ Edge(from, v, w) }
            if rv.isEmpty
          } {
            val residual = edge.w - f(from)(edge.v)
            if (residual > 0 && !acc.contains(edge))
              rv = go(edge.v, to, acc :+ edge)
          }
          rv
        }
      }
      go(source, sink, Vector.empty)
    }

    import annotation.tailrec

    @tailrec def loop(path: Vector[Edge]): Unit = if (path.nonEmpty) {
      val min = path.map(e ⇒ e.w - f(e.u)(e.v)).min
      path foreach {
        case Edge(u, v, _) ⇒
          f(u)(v) = f(u)(v) + min
          f(v)(u) = f(v)(u) - min
      }
      loop(findPath)
    }
    loop(findPath)

    val maxFlow = (g edgesOutOf source map { case (v, _) ⇒ f(source)(v) }).sum

    new Result[Int] {
      def flow = maxFlow
    }
  }

}
