package com.github.skac112.klee.painters

import cats.Id
import com.github.skac112.klee.{Composition, nextGaussBounded}
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.Finger
import com.github.skac112.vgutils.{Angle, Color, ColorVector, Point, ori}

import scala.math.Pi

object FingerSun {
  final case class FingerSunParams(numFinger: Int = 20,
                                   circleRadius: Double = .25,
                                   averageLen: Double = .1,
                                   frontDecayFactor: Double = .9,
                                   backDecayFactor: Double = 5,
                                   sideDecayFactorFactor: Double = 10,
                                   randSeed: Int = 0)
}


import com.github.skac112.klee.painters.FingerSun._

case class FingerSun(params: FingerSunParams, renderParams: Painter.RenderParams) extends
  Painter[FingerSunParams, Id](params, renderParams) {

  lazy val rand = new scala.util.Random(params.randSeed)
  override lazy val img = fun(initImg)
  lazy val initImg = Fill[ColorVector, Id](Color.white)

  lazy val fun = {
    val circle = Circle[ColorVector, Id](ori, params.circleRadius, Color.black)

    val fingers = (0 until params.numFinger) map {i =>
      val angle = Angle(rand.nextDouble() * 2*Pi)
      val from = Point.withAngle(angle, params.circleRadius)

      val to = Point.withAngle(angle, params.circleRadius + nextGaussBounded(rand, .3*params.averageLen,
        1.5*params.averageLen, params.averageLen, .4*params.averageLen))

      Finger[ColorVector, Id](
        from,
        to,
        params.frontDecayFactor,
        params.backDecayFactor,
        params.sideDecayFactorFactor*(from - to).modulus/params.averageLen,
        Finger.DECAY_LIN_SAT,
        Finger.DECAY_LIN_SAT,
        Finger.DECAY_GAUSS)
    }

    Composition[ColorVector, Id](circle :: fingers.toList ::: Nil)
  }
}