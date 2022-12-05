package com.github.skac112.klee.painters

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.{Circle, HalfPlane, Rect}
import com.github.skac112.klee.transforms.displacers.Finger
import com.github.skac112.klee.{Composition, nextDoubleRange, nextGaussBounded}
import com.github.skac112.vgutils.{Angle, Color, ColorVector, Point, ori}

import scala.math.Pi

object FingerComb {
  final case class FingerCombParams(numFinger: Int = 20,
                                   averageLen: Double = .1,
                                   randSeed: Int = 0)
}


import com.github.skac112.klee.painters.FingerComb._

case class FingerComb(params: FingerCombParams, renderParams: Painter.RenderParams) extends
  Painter[FingerCombParams, Id](params, renderParams) {

  lazy val rand = new scala.util.Random(params.randSeed)
  override lazy val img = fun(initImg)
  lazy val initImg = Fill[ColorVector, Id](Color.white)

  lazy val fun = {
    val half_plane = HalfPlane[ColorVector, Id](ori, .5*Pi, Color.red(.7))
//    val half_plane = Rect[ColorVector, Id](Point(0, 1), 10, 2, Color.red(.7))

    val fingers = (0 until params.numFinger) map {i =>
//      val from = Point(-0.259, 0)
      val from = Point(nextDoubleRange(rand, -.5, .5), 0)
//      val to = Point(from.x, -params.averageLen * nextGaussBounded(rand, .3, 1.5, 1, .4))
      val to = Point(from.x, -params.averageLen)

      Finger[ColorVector, Id](
        from,
        to,
        0.5,
        5,
        30*((from-to).modulus)/params.averageLen,
        Finger.DECAY_LIN_SAT,
        Finger.DECAY_LIN_SAT,
        Finger.DECAY_GAUSS)
    }

    Composition[ColorVector, Id](half_plane :: fingers.toList ::: Nil)
  }
}