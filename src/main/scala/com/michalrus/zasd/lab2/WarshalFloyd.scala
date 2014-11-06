package com.michalrus.zasd.lab2

import com.michalrus.zasd.Graph

object WarshalFloyd {

  trait Result[Vertex] {
    def distance(from: Vertex, to: Vertex): Int
    def predecessor(from: Vertex, to: Vertex): Option[Vertex]
    final def shortestPath(from: Vertex, to: Vertex): List[Vertex] = {
      def rpath(last: Vertex): Stream[Vertex] =
        if (last == to) Stream.empty
        else predecessor(from, last) match {
          case None    ⇒ Stream.empty
          case Some(p) ⇒ p #:: rpath(p)
        }
      rpath(to).toList.reverse
    }
  }

  def mutableMap[Vertex](graph: Graph[Vertex, Int]): Result[Vertex] = {
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

    new Result[Vertex] {
      def distance(from: Vertex, to: Vertex): Int = d_predecessor get ((from, to)) map (_._1) getOrElse Int.MaxValue
      def predecessor(from: Vertex, to: Vertex): Option[Vertex] = d_predecessor get ((from, to)) flatMap (_._2)
    }
  }

  def mutableArray(graph: Graph[Int, Int]): Result[Int] = {
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

    new Result[Int] {
      def distance(from: Int, to: Int): Int = res(from)(to)._1
      def predecessor(from: Int, to: Int): Option[Int] = Some(res(from)(to)._2) filter (_ != -1)
    }
  }

  def rawArray(graph: Graph[Int, Int]): Result[Int] = {
    val N = graph.vertices.max
    val d = Array.ofDim[Int](N + 1, N + 1)
    val predec = Array.ofDim[Int](N + 1, N + 1)

    import annotation.tailrec

    @tailrec def init(v1: Int): Unit = {
      @tailrec def loop2(v2: Int): Unit = {
        val e = graph.findEdge(v1, v2)
        if (e.isEmpty) {
          d(v1)(v2) = Int.MaxValue
          predec(v1)(v2) = -1
        }
        else {
          d(v1)(v2) = e.get
          predec(v1)(v2) = v1
        }
        if (v2 < N) loop2(v2 + 1)
      }
      loop2(0)
      if (v1 < N) init(v1 + 1)
    }
    init(0)

    @tailrec def loop1(u: Int): Unit = {
      @tailrec def loop2(v1: Int): Unit = {
        @tailrec def loop3(v2: Int): Unit = {
          val sum = d(v1)(u) + d(u)(v2)
          if (d(v1)(v2) > sum) {
            d(v1)(v2) = sum
            predec(v1)(v2) = predec(u)(v2)
          }
          if (v2 < N) loop3(v2 + 1)
        }
        loop3(0)
        if (v1 < N) loop2(v1 + 1)
      }
      loop2(0)
      if (u < N) loop1(u + 1)
    }
    loop1(0)

    new Result[Int] {
      def distance(from: Int, to: Int): Int = d(from)(to)
      def predecessor(from: Int, to: Int): Option[Int] = Some(predec(from)(to)) filter (_ != -1)
    }
  }

}
