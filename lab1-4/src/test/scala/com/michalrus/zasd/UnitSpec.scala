package com.michalrus.zasd

import org.scalatest.concurrent.Timeouts
import org.scalatest.{ Matchers, WordSpec }
import org.scalatest.prop.Checkers

import scala.io.Source

abstract class UnitSpec extends WordSpec with Checkers with Matchers with Timeouts {

  def populateWith(resource: String)(g: Graph[Int, Int]): Unit =
    GraphParser.addFromString(g, Source.fromURL(getClass.getResource(resource)).mkString)

  def timedWithTime[F](label: String, showInfo: Boolean)(b: ⇒ F): (F, Long) = {
    val start = System.nanoTime
    val f = b
    val stop = System.nanoTime
    val time = stop - start
    if (showInfo) info(s"t_$label = ${time / 1e9} s")
    (f, time)
  }

  def timed[F](label: String)(b: ⇒ F): F = timedWithTime(label, showInfo = true)(b)._1

}
