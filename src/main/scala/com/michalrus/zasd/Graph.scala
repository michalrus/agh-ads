package com.michalrus.zasd

trait Graph[Vertex, EdgeWeight] {

  case class HalfEdge(v: Vertex, weight: EdgeWeight)

  def addVertex(v: Vertex)
  def removeVertex(v: Vertex)
  def addEdge(v: Vertex, w: Vertex, weight: EdgeWeight)
  def removeEdge(v: Vertex, w: Vertex)
  def neighbors(v: Vertex): Set[Vertex]
  def edgesInto(v: Vertex): Set[HalfEdge]
  def edgesOutOf(v: Vertex): Set[HalfEdge]
  def contains(v: Vertex): Boolean
  def findEdge(v: Vertex, w: Vertex): Option[EdgeWeight]
  // „IX. Podaj końce krawędzi”?…
  def vertexCount: Int
  def edgeCount: Int
  final def areAdjacent(v: Vertex, w: Vertex): Boolean = findEdge(v, w).isDefined

}
