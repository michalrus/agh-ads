package com.michalrus.zasd

object GraphParser {

  def addFromString(graph: Graph[Int, Int], input: String): Unit = {
    def parseLine(l: String): (Int, Int, Int) = {
      l.split(",; ".toCharArray).toList filterNot (_.isEmpty) map (_.toInt) match {
        case List(v, w, weight) ⇒ (v, w, weight)
      }
    }

    input.lines map (_.trim) filterNot (_.isEmpty) map parseLine foreach { case (v, w, weight) ⇒ graph.addEdge(v, w, weight) }
  }

}
