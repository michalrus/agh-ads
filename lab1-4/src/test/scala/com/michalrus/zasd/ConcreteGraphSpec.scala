package com.michalrus.zasd

import org.scalacheck.Gen

class MatrixGraphSpec extends GraphSpec(new MatrixGraph[Int],
  sz ⇒ Gen.choose(0, sz),
  Gen.choose(-100, 100))

class AdjacencyGraphSpec extends GraphSpec(new AdjacencyGraph[Int, Int],
  sz ⇒ Gen.choose(0, sz),
  Gen.choose(-100, 100))
