package com.michalrus.zasd.lab6

import com.michalrus.zasd.UnitSpec

import Graham.Point

import scala.io.Source

class GrahamSpec extends UnitSpec {

  def readPoints(resource: String): Set[Point] = {
    def parseLine(ln: String): Point = {
      ln.split(",; ".toCharArray).toList filterNot (_.isEmpty) map (_.toDouble) match {
        case List(x, y) ⇒ Point(x, y)
      }
    }
    Source.fromURL(getClass.getResource(resource)).getLines().map(_.trim).filterNot(_.isEmpty).map(parseLine).toSet
  }

  "Graham" should {
    "work" in {
      val ps = timed("read csv") { readPoints("points.csv") }
      info(s"read ${ps.size} points")
      val res = timed("graham") { Graham.naive(ps) }
      info(s"result = $res")
      info(s"result.size = ${res.size}")
    }
  }

}
