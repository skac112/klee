package com.github.skac112.klee.examples

import com.github.skac112.klee.{Composition, Img, drawToFile}
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Ring
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.vgutils.{Angle, Color, Point}
import scala.math._

class Example1 {
    def blendColors(color1: Color, color2: Color, proportion: Double = .5): Color = {
      val b_col1 = Color(color1.r * proportion, color1.g * proportion, color1.b * proportion)
      val b_col2 = Color(color2.r * (1 - proportion), color2.g * (1 - proportion), color2.b * (1 - proportion))
      b_col1 + b_col2
    }

    def blendColors2(color1: Color, color2: Color, proportion: Double = .5): Color = Color.hsla(
      color1.h + color2.h,
      color1.s * proportion + color2.s * (1 - proportion),
      color1.l * proportion + color2.l * (1 - proportion))

    val c = Point(500, 500)
    val rings = Composition((0 until 9) map {i: Int => Ring(c, 40 + i * 50, 60 + i * 50, Color.red(.7))})
    val count = 5

    val ccf = (srcPt: Point, disp: Point, img: Img) => blendColors2(img(srcPt), img(srcPt + disp), .7)
    val r = 300.0
    val bhs = (0 until count).map {i =>
      val angle = Angle(2 * Pi * i / count)
      val bc1 = BlackHole(c + new Point(r, angle), 10 * Pi, 0.05, 1.0, 0.02, Some(ccf))
      bc1
    }

    def fun = Composition(List(rings, Composition(bhs)))
    drawToFile(fun(Fill(Color.yellow(.7))), "sample38.png", .0, 999.0, .0, 999.0, 1000, 1000)
}
