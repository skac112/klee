package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.gradients.{Angular, Radial}
import com.github.skac112.klee.{Composition, drawToFile, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}

import scala.math._

class Example14 {
  val init_img = Fill[Color, Id](Color.hsla(Angle(Pi), .8, .6))
  val r = .1
    
  val color_fun = (angle: Angle, color: Color) => {
    color.addH(angle)
  }

  val conic_num = 6
  val conics = for {
    i <- 0 until conic_num
    c = Point(.5, .5)
  } yield Angular[Color, Id](c + Point.withAngle(2.0*i/conic_num*Pi, .2), r, color_fun)
//  val conic = Conic[Color, Id](Point(.5, .5), r, color_fun)
  val fun = Composition[Color, Id](conics)
  drawToFile[Color, Id](fun(init_img), trivialColorFun, "example14_3.png", 0, 1, 0, 1, 1000, 1000)
}
