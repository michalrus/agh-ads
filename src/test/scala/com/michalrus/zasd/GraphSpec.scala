package com.michalrus.zasd

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

final class GraphGens[Vertex, EdgeWeight](vertexG: Int ⇒ Gen[Vertex], // Int holds an approximate of the `graph` size
                                          weightG: Gen[EdgeWeight]) {

  val vertices: Gen[Set[Vertex]] =
    Gen.sized(sz ⇒ for {
      realCount ← Gen.choose(0, sz)
      set ← Gen.containerOfN[Set, Vertex](realCount, vertexG(sz))
    } yield set)

  val edges: Gen[Map[(Vertex, Vertex), EdgeWeight]] =
    for {
      vs ← vertices
      es ← Gen.mapOf[(Vertex, Vertex), EdgeWeight](for {
        v ← Gen.oneOf(vs.toSeq)
        w ← Gen.oneOf(vs.toSeq)
        weight ← weightG
      } yield ((v, w), weight))
    } yield es

}

abstract class GraphSpec[Vertex, EdgeWeight](graph: ⇒ Graph[Vertex, EdgeWeight],
                                             vertexG: Int ⇒ Gen[Vertex], // Int holds an approximate of the `graph` size
                                             weightG: Gen[EdgeWeight]) extends UnitSpec {

  val graphGens = new GraphGens(vertexG, weightG)
  import graphGens._

  "A graph" when {

    "empty" should {
      implicit val arbVertex = org.scalacheck.Arbitrary(vertexG(1000))

      "ignore removal of a non-existent vertex" in check { (v: Vertex) ⇒ graph.removeVertex(v); true }
      "ignore removal of a non-existent edge" in check { (v: Vertex, w: Vertex) ⇒ graph.removeEdge(v, w); true }
      "return no edges into" in check { (v: Vertex) ⇒ graph.edgesInto(v).isEmpty }
      "return no edges out of" in check { (v: Vertex) ⇒ graph.edgesOutOf(v).isEmpty }
      "contain no vertices" in check { (v: Vertex) ⇒ !graph.contains(v) }
      "contain no edges" in check { (v: Vertex, w: Vertex) ⇒ !graph.findEdge(v, w).isDefined }
      "report zero vertex count" in { graph.vertexCount shouldBe 0 }
      "report zero edge count" in { graph.edgeCount shouldBe 0 }
    }

    "non-empty" should {

      "report correct vertex count after adding some vertices" in check {
        forAll(vertices) { vs ⇒
          val g = graph
          vs foreach g.addVertex
          g.vertexCount === vs.size
        }
      }

      "report correct vertex count after adding and removing some vertices" in check {
        val gen = for {
          add ← vertices
          rem ← Gen.someOf(add)
        } yield (add, rem)

        forAll(gen) {
          case (add, rem) ⇒
            val g = graph
            add foreach g.addVertex
            rem foreach g.removeVertex
            g.vertexCount === (add.size - rem.size)
        }
      }

      "correctly return from `contains` after `addVertex` and `removeVertex`" in check {
        val gen = for {
          add ← vertices
          rem ← Gen.someOf(add)
        } yield (add, rem)

        forAll(gen) {
          case (add, rem) ⇒
            val g = graph
            add foreach g.addVertex
            rem foreach g.removeVertex
            val cur = add -- rem
            (cur forall g.contains) &&
              (rem forall (v ⇒ !g.contains(v)))
        }
      }

      "report zero edge count when no edges added" in check {
        forAll(vertices) { vs ⇒
          val g = graph
          vs foreach g.addVertex
          g.edgeCount === 0
        }
      }

      "autocreate non-existent vertices of newly added edges" in check {
        forAll(edges) { edges ⇒
          val g = graph
          edges foreach { case ((v, w), weight) ⇒ g.addEdge(v, w, weight) }
          val addedVs = edges.keySet flatMap { case (v, w) ⇒ Set(v, w) }
          g.vertexCount === addedVs.size
        }
      }

      "report correct edge count after adding some edges" in check {
        forAll(edges) { edges ⇒
          val g = graph
          edges foreach { case ((v, w), weight) ⇒ g.addEdge(v, w, weight) }
          g.edgeCount === edges.size
        }
      }

      "report correct edge count after adding and removing some edges" in check {
        val gen = for {
          add ← edges
          rem ← Gen.someOf(add.keySet)
        } yield (add, rem)

        forAll(gen) {
          case (add, rem) ⇒
            val g = graph
            add foreach { case ((v, w), weight) ⇒ g.addEdge(v, w, weight) }
            rem foreach { case (v, w) ⇒ g.removeEdge(v, w) }
            g.edgeCount === add.size - rem.size
        }
      }

      "correctly report edges coming out of a vertex" in check {
        forAll(edges) { edges ⇒
          val g = graph
          edges foreach { case ((v, w), weight) ⇒ g.addEdge(v, w, weight) }
          val grouped = edges groupBy { case ((v, _), _) ⇒ v } mapValues (_.map { case ((_, w), weight) ⇒ g.HalfEdge(w, weight) }.toSet)
          grouped forall { case (v, he) ⇒ g.edgesOutOf(v) == he }
        }
      }

      "correctly report edges coming into a vertex" in check {
        forAll(edges) { edges ⇒
          val g = graph
          edges foreach { case ((v, w), weight) ⇒ g.addEdge(v, w, weight) }
          val grouped = edges groupBy { case ((_, w), _) ⇒ w } mapValues (_.map { case ((v, _), weight) ⇒ g.HalfEdge(v, weight) }.toSet)
          grouped forall { case (w, he) ⇒ g.edgesInto(w) == he }
        }
      }

      "correctly return from `contains` for never added vertices" in check {
        val gen = for {
          edges ← edges
          vertices = edges.keySet flatMap { case (v, w) ⇒ Set(v, w) }
          notAdded ← Gen.containerOf[Set, Vertex](vertexG(1000000) suchThat (v ⇒ !vertices.contains(v)))
        } yield (edges, vertices, notAdded)

        forAll(gen) {
          case (edges, vertices, notAdded) ⇒
            val g = graph
            edges foreach { case ((v, w), weight) ⇒ g.addEdge(v, w, weight) }
            (vertices forall g.contains) && (notAdded forall (v ⇒ !g.contains(v)))
        }
      }

      "correctly return from `contains` for removed vertices" in check {
        val gen = for {
          add ← edges
          rem ← Gen.someOf(add.keySet flatMap { case (v, w) ⇒ Set(v, w) })
        } yield (add, rem)

        forAll(gen) {
          case (add, rem) ⇒
            val g = graph
            add foreach { case ((v, w), weight) ⇒ g.addEdge(v, w, weight) }
            rem foreach g.removeVertex
            val vs = add.keySet flatMap { case (v, w) ⇒ Set(v, w) } filterNot rem.contains
            vs.forall(v ⇒ g.contains(v)) && rem.forall(v ⇒ !g.contains(v))
        }
      }

      "find correct edges after some were removed" in check {
        val gen = for {
          add ← edges
          rem ← Gen.someOf(add.keySet)
          notIn ← Gen.containerOf[Set, (Vertex, Vertex)]((for { v ← vertexG(1000000); w ← vertexG(1000000) } yield (v, w)) suchThat { vw ⇒ !add.contains(vw) })
        } yield (add, rem, notIn)

        forAll(gen) {
          case (add, rem, notIn) ⇒
            val g = graph
            add foreach { case ((v, w), weight) ⇒ g.addEdge(v, w, weight) }
            rem foreach { case (v, w) ⇒ g.removeEdge(v, w) }
            val cur = add -- rem
            (cur forall { case ((v, w), weight) ⇒ g.findEdge(v, w) == Some(weight) }) &&
              (rem forall { case (v, w) ⇒ g.findEdge(v, w) == None }) &&
              (notIn forall { case (v, w) ⇒ g.findEdge(v, w) == None })
        }
      }

    }

  }

}
