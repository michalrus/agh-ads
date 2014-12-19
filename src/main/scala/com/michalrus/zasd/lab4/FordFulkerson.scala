package com.michalrus.zasd.lab4

import com.michalrus.zasd.Graph
import annotation.tailrec

object FordFulkerson {

  trait Result[EdgeWeight] {
    def flow: EdgeWeight
  }

  def arrayTailRec(g: Graph[Int, Int], source: Int, sink: Int): Result[Int] = {
    val N = g.vertices.max + 1
    val f = Array.ofDim[Int](N, N)

    case class Edge(u: Int, v: Int, w: Int)

    def findPath: Vector[Edge] = {
      val predecessors = collection.mutable.HashMap.empty[Int, (Int, Int)] // visited → (its predecessor, edge weight)
      val queue = collection.mutable.Queue.empty[Int] // to-visit
      queue.enqueue(source)
      var found = false
      while (!found && queue.nonEmpty) {
        val u = queue.dequeue()
        if (u == sink) { // a “correct” path was found
          found = true
        }
        else for {
          (v, w) ← g.edgesOutOf(u)
          if !(predecessors contains v) // if not already enqueued
          if w - f(u)(v) > 0 // if the path might be correct
        } {
          predecessors(v) = (u, w)
          queue.enqueue(v)
        }
      }

      var r = Vector.empty[Edge]
      if (found) {
        @tailrec def recAdd(u: Int): Unit = {
          if (u != source) {
            val (v, w) = predecessors(u)
            r :+= Edge(u, v, w)
            recAdd(v)
          }
        }
        recAdd(sink)
      }
      r
    }

    @tailrec def loop(path: Vector[Edge]): Unit =
      if (path.nonEmpty) {
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
