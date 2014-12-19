package com.michalrus.zasd

trait Graph[Vertex, EdgeWeight] {

  def addVertex(v: Vertex)

  def removeVertex(v: Vertex)

  def addEdge(v: Vertex, w: Vertex, weight: EdgeWeight)

  def removeEdge(v: Vertex, w: Vertex)

  // „V. Podaj węzły sąsiednie do węzła”
  // redundant, not implemented;
  //
  //     final def neighbors(v: Vertex) = edgesInto(v) ++ edgesOutOf(v)
  //

  def edgesInto(v: Vertex): Map[Vertex, EdgeWeight]

  def edgesOutOf(v: Vertex): Map[Vertex, EdgeWeight]

  def contains(v: Vertex): Boolean

  def findEdge(v: Vertex, w: Vertex): Option[EdgeWeight]

  // „IX. Podaj końce krawędzi”
  // redundant, not implemented;
  //
  //     final def verticesOfEdge(v: Vertex, w: Vertex) = (v, w)
  //

  def vertexCount: Int

  def edgeCount: Int

  // „XII. Czy węzły są sąsiednie”
  // redundant, not implemented;
  //
  //     final def areAdjacent(v: Vertex, w: Vertex) = findEdge(v, w).isDefined
  //

  def vertices: Set[Vertex]

}
