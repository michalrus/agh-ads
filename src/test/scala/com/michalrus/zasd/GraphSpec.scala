package com.michalrus.zasd

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

abstract class GraphSpec[Vertex, EdgeWeight](graph: => Graph[Vertex, EdgeWeight],
                                             vertexG: Int => Gen[Vertex], // Int holds an approximate of the `graph` size
                                             weightG: Gen[EdgeWeight]) extends UnitSpec {

  "A graph" when {

    "empty" should {
      implicit val arbVertex = org.scalacheck.Arbitrary(vertexG(1000))

      "ignore removal of a non-existent vertex" in check { (v: Vertex) => graph.removeVertex(v); true}
      "ignore removal of a non-existent edge" in check { (v: Vertex, w: Vertex) => graph.removeEdge(v, w); true}
      "return no neighbors" in check { (v: Vertex) => graph.neighbors(v).isEmpty}
      "return no edges into" in check { (v: Vertex) => graph.edgesInto(v).isEmpty}
      "return no edges out of" in check { (v: Vertex) => graph.edgesOutOf(v).isEmpty}
      "contain no vertices" in check { (v: Vertex) => !graph.contains(v)}
      "contain no edges" in check { (v: Vertex, w: Vertex) => !graph.findEdge(v, w).isDefined}
      "report zero vertex count" in { graph.vertexCount shouldBe 0 }
      "report zero edge count" in { graph.edgeCount shouldBe 0 }
      "have no adjacent vertices" in check { (v: Vertex, w: Vertex) => !graph.areAdjacent(v, w)}
    }

    "non-empty" should {

      def vertices: Gen[Set[Vertex]] =
        Gen.sized(sz => for {
          realCount <- Gen.choose(0, sz)
          set <- Gen.containerOfN[Set, Vertex](realCount, vertexG(sz))
        } yield set)

      def edges: Gen[Map[(Vertex, Vertex), EdgeWeight]] =
        for {
          vs <- vertices
          es <- Gen.mapOf[(Vertex, Vertex), EdgeWeight](for {
            v <- Gen.oneOf(vs.toSeq)
            w <- Gen.oneOf(vs.toSeq)
            weight <- weightG} yield ((v, w), weight))
        } yield es

      "report correct vertex count after adding some vertices" in check {
        forAll(vertices) { vs =>
          val g = graph
          vs foreach g.addVertex
          g.vertexCount === vs.size
        }
      }

      "report correct vertex count after adding and removing some vertices" in check {
        def addRem(howMany: Int) = for {
          add <- vertices
          rem <- Gen.someOf(add)
        } yield (add, rem)

        forAll(addRem(1000)) { case (add, rem) =>
          val g = graph
          add foreach g.addVertex
          rem foreach g.removeVertex
          g.vertexCount === (add.size - rem.size)
        }
      }

      "report zero edge count when no edges added" in check {
        forAll(vertices) { vs =>
          val g = graph
          vs foreach g.addVertex
          g.edgeCount === 0
        }
      }

      "autocreate non-existent vertices of newly added edges" in check {
        forAll(edges) { edges =>
          val g = graph
          edges foreach { case ((v, w), weight) => g.addEdge(v, w, weight)}
          val addedVs = edges.keySet flatMap { case (v, w) => Set(v, w)}
          g.vertexCount === addedVs.size
        }
      }

      "report correct edge count after adding some edges" in check {
        forAll(edges) { edges =>
          val g = graph
          edges foreach { case ((v, w), weight) => g.addEdge(v, w, weight)}
          g.edgeCount === edges.size
        }
      }

    }

  }

}
