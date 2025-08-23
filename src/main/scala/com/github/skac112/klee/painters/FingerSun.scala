package com.github.skac112.klee.painters

import cats.Id
import com.github.skac112.klee.nextGaussBounded
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transcomb.{Composition, MultiLocalMixer}
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.{BlackHole, Finger}
import com.github.skac112.vgutils.{Angle, Color, ColorVector, Point, ori}
import com.github.skac112.vgutils.given

import scala.math.Pi

object FingerSun {
  final case class FingerSunParams(numFinger: Int = 20,
                                   circleRadius: Double = .1,
                                   averageLen: Double = .2,
                                   frontDecayFactor: Double = .9,
                                   backDecayFactor: Double = 5,
                                   sideDecayFactorFactor: Double = 15,
                                   randSeed: Int = 0)
}


import com.github.skac112.klee.painters.FingerSun._

case class FingerSun(params: FingerSunParams, renderParams: Painter.RenderParams) extends
  Painter[FingerSunParams, Id](params, renderParams) {

  lazy val rand = new scala.util.Random(params.randSeed)
  override lazy val img = fun(initImg)
  lazy val initImg = Fill[Id](Color.white)

  lazy val fun = {
    val circle = Circle[Id](ori, params.circleRadius, Color.black)

    val fingers_seq = (0 until params.numFinger) map {i =>
      val angle = Angle(rand.nextDouble() * 2*Pi)
      val from = Point.withAngle(angle, params.circleRadius)

      val to = Point.withAngle(angle, params.circleRadius + nextGaussBounded(rand, .1*params.averageLen,
        2.5*params.averageLen, params.averageLen, 1.5*params.averageLen))

      Finger[Id](
        from,
        to,
        params.frontDecayFactor,
        params.backDecayFactor,
        params.sideDecayFactorFactor*(from - to).modulus/params.averageLen,
        Finger.DECAY_LIN_SAT,
        Finger.DECAY_LIN_SAT,
        Finger.DECAY_GAUSS)
    }

    val fingers = MultiLocalMixer[Id](
      fingers_seq,
      (color1, color2) => if (color1.l > color2.l) color2 else color1)

    println(fingers.area.bounds)

    val bhc = 8
    val rbh = .4
    val bhs = for {
      i <- 0 until bhc
      angle = i * 2.0 * Pi / bhc + 1
      rot1 = 3*Pi
      rot = if (i % 2 == 1) rot1 else -rot1
    } yield BlackHole[Id](Point.withAngle(Angle(angle), rbh), rot, 30.0, 1, 0, .1)

//    val elem_tfs = circle :: fingers :: bhs.toList
    val elem_tfs = Seq(circle, fingers)
    Composition[Id](elem_tfs)
  }
}
