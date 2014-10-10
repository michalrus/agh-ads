package com.michalrus.zasd

trait Graph[Vertex, EdgeWeight] {

  case class HalfEdge(v: Vertex, weight: EdgeWeight)

  def addVertex(v: Vertex)

  def removeVertex(v: Vertex)

  def addEdge(v: Vertex, w: Vertex, weight: EdgeWeight)

  def removeEdge(v: Vertex, w: Vertex)

  // Redundant…
  def neighbors(v: Vertex): Set[Vertex] = (edgesInto(v) ++ edgesOutOf(v)) map (_.v)

  def edgesInto(v: Vertex): Set[HalfEdge]

  def edgesOutOf(v: Vertex): Set[HalfEdge]

  def contains(v: Vertex): Boolean

  def findEdge(v: Vertex, w: Vertex): Option[EdgeWeight]

  // „IX. Podaj końce krawędzi”
  // wtf?!

  def vertexCount: Int

  def edgeCount: Int

  // Redundant…
  final def areAdjacent(v: Vertex, w: Vertex): Boolean = findEdge(v, w).isDefined

}
