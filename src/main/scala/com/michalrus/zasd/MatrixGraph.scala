package com.michalrus.zasd

import collection.mutable.ArrayBuffer

final class MatrixGraph extends Graph[Int, Int] {

  private[this] val active = ArrayBuffer.empty[Boolean] // is it NOT deleted?
  private[this] val weights = ArrayBuffer.empty[ArrayBuffer[Option[Int]]]

  def addVertex(v: Int): Unit = {
  }

  def edgeCount: Int = ???

  def edgesInto(v: Int): Set[HalfEdge] = ???

  def edgesOutOf(v: Int): Set[HalfEdge] = ???

  def addEdge(v: Int, w: Int, weight: Int): Unit = ???

  def vertexCount: Int = active count identity

  def removeVertex(v: Int): Unit = {
    if (v < active.size) {
      active(v) = false
      for (i <- 0 until active.size) {
        weights(i)(v) = None
        weights(v)(i) = None
      }
    }
  }

  def findEdge(v: Int, w: Int): Option[Int] = {
    if (contains(v) && contains(w)) weights(v)(w)
    else None
  }

  def contains(v: Int): Boolean = active.

  def removeEdge(v: Int, w: Int): Unit = ???

  def neighbors(v: Int): Set[Int] = ???

}
