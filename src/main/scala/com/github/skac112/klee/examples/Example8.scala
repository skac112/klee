package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFile, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}

import scala.math._

class Example8 {
  val c = Point(.5, .5)
  val init_img = Fill[Color, Id](Color.white)
  val circle = Circle[Color, Color, Id](c, .4, Color.red(.7))
  val bh = BlackHole[Color, Id](c, 10 * Pi, 7, 1.0, 0, .3)   
  drawToFile[Color, Id](bh(circle(init_img)), trivialColorFun, "example8.png", 0, 1, 0, 1, 1500, 1500)
}
