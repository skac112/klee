package com.github.skac112.klee

import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.vgutils.{Angle, Color, Point}
import scala.math._

object Main extends App {
//  def colFun(pt: Point): Color = {
//    val startCol = Color.red(0.8)
//    val mid = Point(500.0, 500.0)
//    val dist = (pt - mid).modulus
//    startCol.addH(.003 * dist)
//  }

  val c = Point(500, 500)
  val r = 350.0
  val circ = Circle(c, r, Color.red(0.8))
//  val black_hole = BlackHole(c + Point(r, 0), 0.1*math.Pi, 0.008, 1.0, 0.02)
//  val bh = new Composition(black_hole, 20)
  val count = 12

  val ccf = (c: Color, disp: Point) => c.addH(Angle(disp.modulus / 30.0))

  val bhs = (0 until count).map {i =>
    val angle = Angle(2 * Pi * i / count)
    BlackHole(c + new Point(r, angle), Pi + 6 * Pi * i / count, 0.05, 1.0, 0.05, Some(ccf))
  }

  def fun = Composition(List(circ, Composition(bhs)))
  render(fun(Fill(Color.white)), "sample21.png", .0, 999.0, .0, 999.0, 1000, 1000)
}
