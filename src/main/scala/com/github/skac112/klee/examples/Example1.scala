package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.area.pt.AxisGrid
import com.github.skac112.klee.{Img, drawToFileOld, trivialColorFun}
import com.github.skac112.klee.images.{Fill, Lines}
import com.github.skac112.klee.transcomb.Composition
import com.github.skac112.klee.transforms.areas.Ring
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.vgutils.{Angle, Color, ColorVector, Point}

import scala.math._

class Example1 {
    def blendColors(color1: Color, color2: Color, proportion: Double = .5): Color = {
      val b_col1 = Color(color1.r * proportion, color1.g * proportion, color1.b * proportion)
      val b_col2 = Color(color2.r * (1 - proportion), color2.g * (1 - proportion), color2.b * (1 - proportion))
      b_col1 + b_col2
    }

    def blendColors2(color1: Color, color2: Color, proportion: Double = .5): Color = ColorVector.hsla(
      color1.h + color2.h,
      color1.s * proportion + color2.s * (1 - proportion),
      color1.l * proportion + color2.l * (1 - proportion))

    val c = Point(.5, .5)
    val rings = (0 until 9) map {i: Int =>
      Ring[Color, Id](c, .04 + i * .05, .06 + i * .05, Color.red(.7))
    }
    val count = 5
//    val ccf = (srcPt: Point, disp: Point, img: Img) => blendColors2(img(srcPt), img(srcPt + disp), .7)
    val r = .3
    val bhs = (0 until count).map { i =>
      val angle = Angle(2 * Pi * i / count)
      BlackHole[Color, Id](c + new Point(r, angle), 3 * Pi, 30, 1.0, 0, .2)
    }

//    def init_img = Lines[Color, Id](0, 0, .05, 0.05, .01, Color.black, Color.white)

    def fun = Composition[Color, Id](rings ++ bhs)
    drawToFileOld[Color, Id](fun(Fill(Color.yellow(.7))), trivialColorFun, "example1_5.png", 0, 1, 0, 1, 400, 400)
}
