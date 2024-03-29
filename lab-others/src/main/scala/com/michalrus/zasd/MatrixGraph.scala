package com.michalrus.zasd

final class MatrixGraph[EdgeWeight] extends Graph[Int, EdgeWeight] {

  import collection.mutable.ArrayBuffer

  private[this] val active = ArrayBuffer.empty[Boolean] // is it NOT deleted?
  private[this] val weights = ArrayBuffer.empty[ArrayBuffer[Option[EdgeWeight]]]

  def addVertex(v: Int): Unit = {
    if (!contains(v) && v >= 0) {
      if (v < active.size) {
        // recreate → make sure there are no leftovers…
        active(v) = true
        for (w ← 0 until active.size) {
          weights(v)(w) = None
          weights(w)(v) = None
        }
      }
      else {
        // resize buffers
        for (i ← 0 until active.size) {
          weights(i).sizeHint(v)
          weights(i) ++= ArrayBuffer.fill(v + 1 - active.size)(Option.empty[EdgeWeight])
        }
        weights.sizeHint(v)
        active.sizeHint(v)
        for (i ← active.size to v) {
          active += false
          weights += ArrayBuffer.fill(v + 1)(Option.empty[EdgeWeight])
        }
        active(v) = true
      }
    }
  }

  def edgeCount: Int = weights.map(_ count (_.isDefined)).sum

  def edgesInto(v: Int): Map[Int, EdgeWeight] = {
    if (contains(v))
      (for {
        w ← 0 until active.size
        if contains(w)
        weight ← weights(w)(v)
      } yield w → weight).toMap
    else Map.empty
  }

  def edgesOutOf(v: Int): Map[Int, EdgeWeight] = {
    if (contains(v))
      (for {
        w ← 0 until active.size
        if contains(w)
        weight ← weights(v)(w)
      } yield w → weight).toMap
    else Map.empty
  }

  def addEdge(v: Int, w: Int, weight: EdgeWeight): Unit = {
    addVertex(v)
    addVertex(w)
    if (contains(v) && contains(w)) weights(v)(w) = Some(weight)
  }

  def vertexCount: Int = active count identity

  def removeVertex(v: Int): Unit = {
    if (contains(v)) {
      active(v) = false
      for {
        w ← 0 until active.size
        if contains(w)
      } {
        weights(w)(v) = None
        weights(v)(w) = None
      }
    }
  }

  def findEdge(v: Int, w: Int): Option[EdgeWeight] = {
    if (contains(v) && contains(w)) weights(v)(w)
    else None
  }

  def contains(v: Int): Boolean = v >= 0 && v < active.size && active(v)

  def removeEdge(v: Int, w: Int): Unit =
    if (contains(v) && contains(w)) weights(v)(w) = None

  def vertices: Set[Int] = (active.zipWithIndex filter (_._1) map (_._2)).toSet

}
