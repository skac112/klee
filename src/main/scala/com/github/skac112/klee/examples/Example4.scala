package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Ring
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFileOld, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import scala.math._
import com.github.skac112.klee.images.Lines
import com.github.skac112.klee.ImgTrans

class Example4 {
  val c = Point(.5, .5)
  val init_img = Lines[Color, Id](0, 0, .03, 0.03, .005, Color.black, Color.white)
  val bh1 = BlackHole[Color, Id](c + Point(.2, .2), 3 * Pi, 10, 1.0, 0, .7)
  val bh2 = BlackHole[Color, Id](c - Point(.2, .2), 3 * Pi, 10, 1.0, 0, .7)
  val ring1 = Ring[Color, Id](c + Point(.1, .1), .2, .25, Color.blue(.7))
  val ring2 = Ring[Color, Id](c - Point(.1, .1), .2, .25, Color.red(.7))
  val ring3 = Ring[Color, Id](c - Point(.1, .1), .3, .35, Color.green(.7))
  val comp = Composition(List[ImgTrans[Color, Color, Id]](ring2, ring1, ring3, bh1, bh2))
  drawToFileOld[Color, Id](comp(init_img), trivialColorFun, "example4_3.png", 0, 1, 0, 1, 1500, 1500)
//   drawToFile[Color, Id](bh(ring2(ring1(init_img))), trivialColorFun, "example3_2.png", 0, 1, 0, 1, 1500, 1500)
}
