package com.michalrus.zasd.lab6

import scala.collection.mutable.Stack

object Graham {

  case class Point(x: Double, y: Double) {
    def -(that: Point) = Point(this.x - that.x, this.y - that.y)
  }

  def naive(q: Set[Point]): List[Point] = {

    require(q.size >= 3, "Input size should be ≥ 3")

    // http://wazniak.mimuw.edu.pl/index.php?title=Zaawansowane_algorytmy_i_struktury_danych/Wyk%C5%82ad_11#Algorytm_Grahama

    // 1  niech p_0 będzie punktem z Q o najmniejszej współrzędnej y, jeżeli
    //    jest kilka takich punktów, to tym najbardziej na lewo spośród nich,

    val p0 = q minBy (p ⇒ (p.y, p.x))

    // 2  posortujmy pozostałe punkty z Q malejąco po ich współrzędnych polarnych względem p_0,
    //    niech (p_1,\ldots,p_n) będzie tym posortowanym ciągiem,

    val ps = ((q - p0).toList groupBy (p ⇒ math.atan2(p.y - p0.y, p.x - p0.x))

      // 3  jeżeli w ciągu (p_1, \ldots, p_n) występują dwa punkty o takiej samej współrzędnej polarnej,
      //    to pozostaw tylko jeden najbardziej odległy od p_0, niech (p_1,\ldots,p_m) będzie
      //    pozostałym ciągiem punktów,

      mapValues (_ maxBy (p ⇒ p.x * p.x + p.y * p.y))).toList.sortBy(_._1).map(_._2)

    // 4  PUSH(p_0,S)
    // 5  PUSH(p_1,S)
    // 6  PUSH(p_2,S)

    val st: Stack[Point] = Stack.empty[Point]
    val (p1 :: p2 :: p_rest) = ps
    st.push(p0, p1, p2)

    def toTheRight(p: Point): Boolean = {
      // punkt p_i jest na prawo wektora TOP(S) --> NEXT-TO-TOP(S)
      val p_top = st.top
      val p_nexttotop = st.tail.top

      def det(p1: Point, p2: Point): Double = p1.x * p2.y - p2.x * p1.y
      val d = det(p_nexttotop - p_top, p - p_top)
      d > 0
    }

    // 7  for i=3 to m do
    // 8  begin
    for (p_i ← p_rest) {
      // 9    while punkt p_i jest na prawo wektora TOP(S)\to NEXT-TO-TOP(S)  do
      while (toTheRight(p_i))
        // 10     POP(S)
        st.pop()
      // 11   PUSH(p_i,S)
      st.push(p_i)
    }
    // 12 end
    // 13 return S

    st.toList
  }

}
