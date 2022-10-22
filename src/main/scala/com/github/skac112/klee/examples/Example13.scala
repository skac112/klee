package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFile, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import com.github.skac112.vgutils._
import scala.math._
import com.github.skac112.klee.transforms.gradients.Ring
import com.github.skac112.klee.transforms.gradients.Radial

class Example13 {
  val rand = new scala.util.Random(11)
  val init_img = Fill[Color, Id](Color.hsla(Angle(Pi), .8, .1))
  val a = 5.0
  val r = 12.0/a
  val omega_f = 10
    
  val color_fun = (d: Double, color: Color) => {
    val k = exp(-a*d)*cos(omega_f*a*sqrt(d))
    val l_ch = k/1.1
    val h_ch = k*1.5
    val s_ch = k/3.0            
    color.addH(Angle(h_ch)).addS(s_ch).addL(l_ch)            
  }

  val ring = Radial[Color, Id](Point(.5, .5), r, color_fun)
  drawToFile[Color, Id](ring(init_img), trivialColorFun, "example13_8.png", 0, 1, 0, 1, 1000, 1000)
}
