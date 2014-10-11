package com.michalrus.zasd

import org.scalacheck.Prop.{forAll, BooleanOperators}
import org.scalacheck.Gen

abstract class GraphSpec[Weight](gen: => Graph[Int, Weight], weights: Gen[Weight]) extends UnitSpec {

  "A graph" when {

    "empty" should {
      "ignore removal of a non-existent vertex" in check { (v: Int) => gen.removeVertex(v); true}
      "ignore removal of a non-existent edge" in check { (v: Int, w: Int) => gen.removeEdge(v, w); true}
      "return no neighbors" in check { (i: Int) => gen.neighbors(i).isEmpty}
      "return no edges into" in check { (i: Int) => gen.edgesInto(i).isEmpty}
      "return no edges out of" in check { (i: Int) => gen.edgesOutOf(i).isEmpty}
      "contain no vertices" in check { (i: Int) => !gen.contains(i)}
      "contain no edges" in check { (v: Int, w: Int) => !gen.findEdge(v, w).isDefined}
      "report zero vertex count" in { gen.vertexCount shouldBe 0 }
      "report zero edge count" in { gen.edgeCount shouldBe 0 }
      "have no adjacent vertices" in check { (v: Int, w: Int) => !gen.areAdjacent(v, w)}
    }

    "non-empty" should {

      def vertices: Gen[Set[Int]] =
        Gen.sized(sz => for {
          count <- Gen.choose(0, sz)
          set <- Gen.containerOfN[Set, Int](count, Gen.choose(0, sz))
        } yield set)

      def edges: Gen[Map[(Int, Int), Weight]] =
        for {
          vs <- vertices
          es <- Gen.mapOf[(Int, Int), Weight](for {
            v <- Gen.oneOf(vs.toSeq)
            w <- Gen.oneOf(vs.toSeq)
            weight <- weights } yield ((v, w), weight))
        } yield es

      "report correct vertex count after adding some vertices" in check {
        forAll(vertices) { vs =>
          val g = gen
          vs foreach g.addVertex
          g.vertexCount === vs.size
        }
      }

      "report correct vertex cound after adding and removing some vertices" in check {
        def addRem(howMany: Int) = for {
          add <- vertices
          rem <- Gen.someOf(add)
        } yield (add, rem)

        forAll(addRem(1000)) { case (add, rem) =>
          val g = gen
          add foreach g.addVertex
          rem foreach g.removeVertex
          g.vertexCount === (add.size - rem.size)
        }
      }

      "report zero edge count when no edges added" in check {
        forAll(vertices) { vs =>
          val g = gen
          vs foreach g.addVertex
          g.edgeCount === 0
        }
      }

      "autocreate non-existent vertices of newly added edges" in check {
        forAll(edges) { edges =>
          val g = gen
          edges foreach { case ((v, w), weight) => g.addEdge(v, w, weight) }
          val addedVs = edges.keySet flatMap { case (v, w) => Set(v, w) }
          g.vertexCount === addedVs.size
        }
      }

      "report correct edge count after adding some edges" in check {
        forAll(edges) { edges =>
          val g = gen
          edges foreach { case ((v, w), weight) => g.addEdge(v, w, weight) }
          g.edgeCount === edges.size
        }
      }

    }

  }

}
