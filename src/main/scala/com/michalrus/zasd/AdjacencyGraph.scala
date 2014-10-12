package com.michalrus.zasd

final class AdjacencyGraph[Vertex, EdgeWeight] extends Graph[Vertex, EdgeWeight] {

  import collection.mutable
  private[this] val adjacency = mutable.HashMap.empty[Vertex, mutable.HashMap[Vertex, EdgeWeight]]

  def addVertex(v: Vertex): Unit = {
    if (!adjacency.contains(v)) adjacency += v -> mutable.HashMap.empty
    ()
  }

  def removeVertex(v: Vertex): Unit = {
    adjacency -= v
    for {
      w ← adjacency.keySet
      adj ← adjacency get w
    } adj -= v
  }

  def addEdge(v: Vertex, w: Vertex, weight: EdgeWeight): Unit = {
    addVertex(v); addVertex(w)
    adjacency get v foreach (_ += w → weight)
  }

  def removeEdge(v: Vertex, w: Vertex): Unit = {
    adjacency get v foreach (_ -= w)
  }

  def edgesInto(u: Vertex): Set[HalfEdge] = {
    (for {
      (v, adj) ← adjacency
      (w, weight) ← adj
      if w == u
    } yield HalfEdge(v, weight)).toSet
  }

  def edgesOutOf(v: Vertex): Set[HalfEdge] = {
    val map = (adjacency get v).toSet
    for {
      adj ← map
      (w, weight) ← adj
    } yield HalfEdge(w, weight)
  }

  def contains(v: Vertex): Boolean = adjacency.contains(v)

  def findEdge(v: Vertex, w: Vertex): Option[EdgeWeight] = {
    for {
      adj ← adjacency get v
      he ← adj get w
    } yield he
  }

  def vertexCount: Int = adjacency.size

  def edgeCount: Int = adjacency.values.map(_.size).sum

}
