package com.michalrus.zasd

import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

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
        val input =
          """9, 6, 56
            |15, 11, 149
            |1, 3, 154
            |15, 4, 52
            |19, 12, 60
            |14, 17, 8
            |2, 9, 102
            |5, 8, 11
            |10, 20, 194
            |19, 20, 2
            |13, 8, 149
            |2, 7, 69
            |15, 2, 79
            |10, 4, 188
            |3, 6, 26
            |13, 17, 16
            |15, 7, 177
            |18, 3, 10
            |2, 15, 180
            |9, 20, 44
            |7, 1, 107
            |6, 7, 122
            |19, 18, 178
            |17, 6, 165
            |20, 12, 31
            |16, 18, 155
            |6, 14, 123
            |5, 10, 48
            |1, 17, 63
            |17, 4, 119
            |3, 11, 200
            |9, 5, 125
            |19, 20, 72
            |8, 12, 51
            |1, 11, 77
            |10, 20, 94
            |13, 8, 169
            |15, 10, 129
            |7, 18, 184
            |18, 2, 21
            |8, 4, 124
            |14, 1, 151
            |20, 19, 79
            |20, 13, 155
            |6, 17, 66
            |7, 13, 27
            |6, 16, 9
            |3, 10, 52
            |16, 2, 127
            |2, 4, 96
            |17, 11, 181
            |8, 9, 199
            |16, 15, 29
            |5, 12, 125
            |5, 14, 28
            |14, 6, 129
            |15, 12, 133
            |15, 1, 50
            |5, 2, 114
            |12, 2, 23
            |8, 17, 196
            |15, 3, 89
            |3, 2, 89
            |5, 18, 134
            |7, 12, 193
            |20, 10, 49
            |20, 17, 24
            |16, 2, 122
            |20, 1, 176
            |17, 20, 77
            |18, 5, 190
            |6, 3, 192
            |8, 20, 139
            |13, 19, 61
            |17, 12, 66
            |13, 3, 80
            |7, 9, 35
            |17, 5, 41
            |2, 7, 42
            |4, 10, 190
            |11, 3, 55
            |6, 16, 141
            |19, 13, 45
            |18, 20, 153
            |19, 18, 124
            |14, 9, 21
            |2, 15, 191
            |9, 12, 29
            |16, 18, 41
            |10, 5, 89
            |19, 8, 173
            |6, 11, 72
            |2, 9, 70
            |1, 16, 164
            |1, 19, 147
            |18, 5, 177
            |19, 5, 137
            |16, 15, 54
            |16, 17, 87
            |1, 7, 52
          """.stripMargin
        GraphParser.addFromString(g, input)

        // check 3 random edges
        g.findEdge(19, 8) shouldBe Some(173)
        g.findEdge(3, 2) shouldBe Some(89)
        g.findEdge(2, 3) should not be Some(89)
        g.findEdge(2, 3) shouldBe None
      }

    }
  }
}