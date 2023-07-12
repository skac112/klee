package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transcomb.Composition
import com.github.skac112.klee.transforms.gradients.{Radial, Ring}
import com.github.skac112.klee.{blendColors, drawToFileOld, oppositeHLColor, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, ColorVector, Point}

import scala.math._

class Example16 {
  def addToColor(color: Color, rAdd: Double, gAdd: Double, bAdd: Double) = {
    def addPart(part: Double, add: Double): Double = part + add match {
      case value if (value < 0) => -value - floor(abs(value))
//      case value if (value < 0) => 0
      case 0.0 => 0
      case 1.0 => 1
      case value if (value > 1) => ceil(value) - value
//      case value if (value > 1) => 1
      case value => value
    }

    Color(addPart(color.r, rAdd), addPart(color.g, gAdd), addPart(color.b, bAdd))
  }

  println(getClass.getName)
  val rand = new scala.util.Random(2)
  val init_img = Fill[Color, Id](ColorVector.hsla(Angle(Pi), .7, .8))

//   val color_fun = (d: Double, color: Color) => {
//     val a = 10
//     val b = .1
//     val k = 1.0/(d*a + b)*b
//     color.addL(k/3).addH(k/2)   
//   }

  val elements = for {
    i <- 0 until 300
    a = math.abs(10 + 40*rand.nextGaussian())
//    a = 10
    r = 12.0/a
    b = math.abs(rand.nextGaussian() * .1)
//    b = .5
//    rr = max(0, 0.01 + .02 * rand.nextGaussian)
    disperse_f = .8
    r_add = min(1, max(-1, disperse_f*rand.nextGaussian()))
    g_add = min(1, max(-1, disperse_f*rand.nextGaussian()))
    b_add = min(1, max(-1, disperse_f*rand.nextGaussian()))
    color_fun = (d: Double, color: Color) => {
//         val k = b/(d*d*a*a + b)
        val k = b / (b + d*d*a*a + .3*d*d*d*a*a*a*(1 + math.sin(d*a*a)))
        val second_color = addToColor(color, r_add, g_add, b_add)
        blendColors(second_color, color, k).toColor()
//        second_color
    }

    element = Radial[Color, Id](Point(rand.nextDouble, rand.nextDouble), r, color_fun)
//    element = Radial[Color, Id](Point(.5, .5), r, color_fun)
  } yield element

  val fun = Composition[Color, Id](elements.toList)
//   val fun = Composition[Color, Id](rings.toList ::: bh :: Nil)
  drawToFileOld[Color, Id](fun(init_img), trivialColorFun, "example16_4.png", 0, 1, 0, 1, 1000, 1000)
}
