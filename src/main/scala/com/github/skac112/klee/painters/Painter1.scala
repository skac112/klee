package com.github.skac112.klee.painters

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transcomb.Composition
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Img, blendColors, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import com.github.skac112.vgutils._
import scala.math._
import com.github.skac112.klee.transforms.gradients.Ring
import com.github.skac112.klee.transforms.gradients.Radial
import com.github.skac112.vgutils.given

object Painter1 {
  final case class Painter1Params(
    randSeed: Int,
    numElems: Int,
    aBase: Double,
    aSpan: Double,
    colorDisperse: Double)
}

import Painter1._

final case class Painter1(params: Painter1Params, renderParams: Painter.RenderParams) extends
  Painter[Painter1Params, Id](params, renderParams) {
  override lazy val img = fun(initImg)

  lazy val initImg = Fill[Id](ColorVector.hsla(Angle(Pi), .7, .8))

  lazy val fun = {
    val elements = for {
      i <- 0 until params.numElems
      a = math.abs(params.aBase + params.aSpan*rand.nextGaussian())
      //    a = 10
      r = 12.0/a
      b = math.abs(rand.nextGaussian() * .1)
      //    b = .5
      //    rr = max(0, 0.01 + .02 * rand.nextGaussian)
      disperse_f = .8
      r_add = min(1, max(-1, params.colorDisperse*rand.nextGaussian()))
      g_add = min(1, max(-1, params.colorDisperse*rand.nextGaussian()))
      b_add = min(1, max(-1, params.colorDisperse*rand.nextGaussian()))
      color_fun = (d: Double, color: ColorVector) => {
        //         val k = b/(d*d*a*a + b)
        val k = b / (b + d*d*a*a + .3*d*d*d*a*a*a*(1 + math.sin(d*a*a)))
        val second_color = addToColor(color, r_add, g_add, b_add)
        blendColors(second_color, color, k)
      }

      element = Radial[Id](Point(rand.nextDouble, rand.nextDouble), r, color_fun)
      //    element = Radial[Color, Id](Point(.5, .5), r, color_fun)
    } yield element

    Composition[Id](elements.toList)
  }

  def addToColor(color: ColorVector, rAdd: Double, gAdd: Double, bAdd: Double) = {
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

  lazy val rand = new scala.util.Random(params.randSeed)
}
