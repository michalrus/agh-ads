package com.michalrus.zasd

import org.scalacheck.Gen

class MatrixGraphSpec extends GraphSpec(new MatrixGraph[Int], Gen.choose(-100, 100))
