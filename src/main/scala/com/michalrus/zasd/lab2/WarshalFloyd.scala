package com.michalrus.zasd.lab2

import com.michalrus.zasd.Graph

object WarshalFloyd {

  def mutableMap[Vertex](graph: Graph[Vertex, Int]): Map[(Vertex, Vertex), (Int, Option[Vertex])] = {
    import collection.mutable
    val vs = graph.vertices

    val d_predecessor: mutable.HashMap[(Vertex, Vertex), (Int, Option[Vertex])] = (for { // FIXME: naming
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
      val uv2 = d_predecessor((u, v2))
      val candidate = d_predecessor((v1, u))._1 + uv2._1
      if (d_predecessor((v1, v2))._1 > candidate)
        d_predecessor((v1, v2)) = (candidate, uv2._2)
      Thread.sleep(0)
    }

    d_predecessor.toMap
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

  def rawArray(graph: Graph[Int, Int]): (Array[Array[Int]], Array[Array[Int]]) = {
    val N = graph.vertices.max + 1
    val d = Array.ofDim[Int](N, N)
    val predecessor = Array.ofDim[Int](N, N)

    var v1, v2, u = 0
    var e = Option.empty[Int]

    v1 = 0
    while (v1 < N) {
      v2 = 0
      while (v2 < N) {
        e = graph.findEdge(v1, v2)
        if (e.isEmpty) {
          d(v1)(v2) = Int.MaxValue
          predecessor(v1)(v2) = -1
        }
        else {
          d(v1)(v2) = e.get
          predecessor(v1)(v2) = v1
        }
        v2 += 1
      }
      d(v1)(v1) = 0
      v1 += 1
    }

    var sum = 0
    u = 0
    while (u < N) {
      v1 = 0
      while (v1 < N) {
        v2 = 0
        while (v2 < N) {
          sum = d(v1)(u) + d(u)(v2)
          if (d(v1)(v2) > sum) {
            d(v1)(v2) = sum
            predecessor(v1)(v2) = predecessor(u)(v2)
          }
          v2 += 1
        }
        v1 += 1
      }
      u += 1
    }

    (d, predecessor)
  }

}
