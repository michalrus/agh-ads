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

}
