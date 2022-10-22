package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFile, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import com.github.skac112.vgutils._

import scala.math._

class Example8 {
  val c = Point(.5, .5)
  val init_img = Fill[Color, Id](Color.white)
  val r = .2
  val circle = Circle[Color, Id](c, .2, Color.red(.7))
  val d = Point(r, 0)
  val bh1 = BlackHole[Color, Id](c + d, 5 * Pi, 10, 1.0, 0, .9) 
  val bh2 = BlackHole[Color, Id](c + (d.rot(120.deg)), 5 * Pi, 10, 1.0, 0, .9)
  val bh3 = BlackHole[Color, Id](c + (d.rot(240.deg)), 5 * Pi, 10, 1.0, 0, .9)   
  drawToFile[Color, Id](bh3(bh2(bh1(circle(init_img)))), trivialColorFun, "example8_5.png", 0, 1, 0, 1, 1500, 1500)
}
