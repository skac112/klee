package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Ring
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{drawToFileOld, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import scala.math._
import com.github.skac112.klee.images.Lines
import com.github.skac112.klee.ImgTrans
import com.github.skac112.klee.transcomb.Composition
import com.github.skac112.vgutils.Angle._
import com.github.skac112.vgutils._

class Example5 {
  val c = Point(.5, .5)
//   val init_img = Lines[Color, Id](0, 0, .03, 0.03, .005, Color.black, Color.white)
  val init_img = Fill[Color, Id](Color.white)
  val r = .16
  val w = .1
  val k = -.12
  val p1 = Point(sqrt((r + k)/2.0), sqrt((r + k)/2.0))
  val bh = BlackHole[Color, Id](c - (p1 * .5), 20 * Pi, 17, 1.0, 0, 1)
//  val p2 = p1 rot (120.0.deg)s
//   val p3 = p1 rot (240.0.deg)  
  val ring1 = Ring[Color, Id](c + (p1 * .5), r - .5*w, r + .5*w, Color.blue(.7))
//   val ring2 = Ring[Color, Color, Id](c + p2, r - .5*w, r + .5*w, Color.red(.7))
//   val ring3 = Ring[Color, Color, Id](c + p3, r - .5*w, r + .5*w, Color.green(.7))

  val comp = Composition(ring1 :: bh :: Nil)
  drawToFileOld[Color, Id](comp(init_img), trivialColorFun, "example5_23.png", 0, 1, 0, 1, 1500, 1500)
//   drawToFile[Color, Id](bh(ring2(ring1(init_img))), trivialColorFun, "example3_2.png", 0, 1, 0, 1, 1500, 1500)
}
