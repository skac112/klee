package com.github.skac112.klee.painters

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transcomb.Composition
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.vgutils.{Angle, Color, ColorVector, Point, ori}
import com.github.skac112.vgutils.given

import scala.math.Pi

object BlackHoles {
  final case class Params(numHoles: Int = 10, rotAngle: Double = 10 * Pi)
}

import BlackHoles._

case class BlackHoles(params: Params, renderParams: Painter.RenderParams)
  extends Painter[BlackHoles.Params, Id](params, renderParams) {
  override lazy val img = fun(initImg)
  val initImg = Fill[Id](Color.white)
  val r = .2
  val circle = Circle[Id](ori, .2, Color.red(.7))

  val bhs = for {
    i <- 0 until params.numHoles
    angle = i * 2.0 * Pi / params.numHoles
    // rot = if (i % 2 == 1) rot1 else -rot1
  } yield BlackHole[Id](Point.withAngle(Angle(angle), r), params.rotAngle, 15.0, 1, 0, .5)

  val fun = Composition[Id](circle :: bhs.toList)
}
