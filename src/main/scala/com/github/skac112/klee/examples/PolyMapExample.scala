package com.github.skac112.klee.examples

import com.github.skac112.klee.dynsys.{PolyMap, taylorExp}
import com.github.skac112.vgutils.Point

class PolyMapExample {
  val p = new PolyMap {
    override val initCoeffs = Seq(
      Seq(Point(1, 1), Point(2, 0)),
      Seq(Point(0, 2), Point(-3, 1)),
      Seq(Point(2, 3)))
  }
  val t = taylorExp(p, Point(2.0, 1.0), 2, 1e-5)
  println(p(Point(3.0, 3.0)))
  println(t(Point(3.0, 3.0)))
}
