package com.michalrus.zasd

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

import scala.io.Source

class GraphParserSpec extends UnitSpec {

  val graphGens = new GraphGens(sz ⇒ Gen.choose(0, sz), Gen.choose(-100, 100))
  import graphGens._

  "GraphParser" when {

    "testing `addFromString`" should {

      "correctly parse some random examples" in check {
        forAll(edges) { edges ⇒
          val input = edges map { case ((v, w), weight) ⇒ s"$v, $w, $weight" } mkString "\n"
          val g = new MatrixGraph[Int]
          GraphParser.addFromString(g, input)
          edges forall { case ((v, w), weight) ⇒ g.findEdge(v, w) == Some(weight) }
        }
      }

      "parse dr Sędziwy's file" in {
        val g = new MatrixGraph[Int]
        val input = Source.fromURL(getClass.getResource("lab1/graph.txt")).mkString
        GraphParser.addFromString(g, input)

        // check 3 random edges
        g.findEdge(19, 11) shouldBe Some(155)
        g.findEdge(3, 9) shouldBe Some(170)
        g.findEdge(2, 2) should not be Some(89)
        g.findEdge(2, 2) shouldBe None
      }

    }
  }
}
