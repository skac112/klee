package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transcomb.Composition
import com.github.skac112.klee.transforms.gradients.{Angular, Polar}
import com.github.skac112.klee.{blendColors, drawToFileOld, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, ColorVector, Point}

import scala.math._

class Example15 {
  val init_img = Fill[Color, Id](ColorVector.hsla(Angle(Pi), .8, .2))
  val a = 10.0
  val r = 12.0/a
  val omega_f = 0

  val color_fun = (radius: Double, angle: Angle, color: Color) => {
    val strength = 2 * exp(-a*radius)*cos(omega_f*a*sqrt(radius))
//    color.addH(cos(angle) * strength).addL(-.2 * strength)
//    val ch_col = color.addH(angle * 3)
    val ch_col = color.addH(angle*.5).addL(.3 * strength)
    blendColors(ch_col, color, strength).toColor()
  }

  val conic_num = 6
//  val conics = for {
//    i <- 0 until conic_num
//    c = Point(.5, .5)
//  } yield Polar[Color, Id](c + Point.withAngle(2.0*i/conic_num*Pi, .2), r, color_fun)
  val fun = Polar[Color, Id](Point(.5, .5), r, color_fun)
//  val fun = Composition[Color, Id](conics)
  drawToFileOld[Color, Id](fun(init_img), trivialColorFun, "example15_9.png", 0, 1, 0, 1, 1000, 1000)
}
