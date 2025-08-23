package com.github.skac112.klee.painters

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.painters.Painter1.Painter1Params
import com.github.skac112.klee.transcomb.Composition
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.Finger
import com.github.skac112.vgutils.{Angle, Color, ColorVector, Point, ori}
import com.github.skac112.vgutils.given
import scala.math.Pi

object Painter2 {
  final case class Painter2Params(from: Point,
                                  to: Point,
                                  frontDecay: Double,
                                  backDecay: Double,
                                  sideDecay: Double,
                                  frontDecayType: Symbol,
                                  backDecayType: Symbol,
                                  sideDecayType: Symbol)
}

import Painter2._
import Painter._

case class Painter2(params: Painter2Params, renderParams: Painter.RenderParams) extends
  Painter[Painter2Params, Id](params, renderParams) {

  override lazy val img = fun(initImg)

  lazy val initImg = Fill[Id](Color.white)

  lazy val fun = {
    val circle = Circle[Id](ori, .1, Color.black)

    val finger = Finger[Id](
      params.from,
      params.to,
      params.frontDecay,
      params.backDecay,
      params.sideDecay,
      params.frontDecayType,
      params.backDecayType,
      params.sideDecayType)

    Composition[Id](circle :: finger :: Nil)
  }
}