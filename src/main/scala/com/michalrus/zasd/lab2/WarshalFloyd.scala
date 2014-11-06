package com.michalrus.zasd.lab2

import com.michalrus.zasd.Graph

object WarshalFloyd {

  def mutableMap[Vertex](graph: Graph[Vertex, Int]): Map[(Vertex, Vertex), (Int, Option[Vertex])] = {
    import collection.mutable
    val vs = graph.vertices

    val dPoprzednik: mutable.HashMap[(Vertex, Vertex), (Int, Option[Vertex])] = (for { // FIXME: naming
      v1 ← vs
      v2 ← vs
    } yield (v1, v2) → {
      if (v1 == v2) (0, None)
      else (graph findEdge (v1, v2) getOrElse Int.MaxValue, Some(v1)) // FIXME: Int.MaxValue
    })(collection.breakOut)

    for {
      u ← vs
      v1 ← vs
      v2 ← vs
    } {
      val uv2 = dPoprzednik((u, v2))
      val candidate = dPoprzednik((v1, u))._1 + uv2._1
      if (dPoprzednik((v1, v2))._1 > candidate)
        dPoprzednik((v1, v2)) = (candidate, uv2._2)
      Thread.sleep(0)
    }

    dPoprzednik.toMap
  }

  def mutableArray(graph: Graph[Int, Int]): Array[Array[(Int, Int)]] = {
    val vs = graph.vertices
    val N = vs.max + 1
    val res = Array.ofDim[(Int, Int)](N, N)

    for (v1 ← vs) {
      for (v2 ← vs) {
        res(v1)(v2) = graph.findEdge(v1, v2) match {
          case Some(w) ⇒ (w, v1)
          case None    ⇒ (Int.MaxValue, -1)
        }
      }
      res(v1)(v1) = (0, -1)
    }

    for (u ← vs; v1 ← vs; v2 ← vs) {
      val sum = res(v1)(u)._1 + res(u)(v2)._1
      if (res(v1)(v2)._1 > sum)
        res(v1)(v2) = (sum, res(u)(v2)._2)
    }

    res
  }

}
