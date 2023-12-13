package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.area.img.{ImgArea, Rect}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.transforms.displacers.Finger.{DECAY_EXP, DECAY_GAUSS, DECAY_LIN_SAT}
import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.transform.PivotRot

import scala.math.{abs, exp, log, max, sqrt}

case class ExpandingRing[I, M[_]](center: Point,
                                  maxDispRadius: Point,
                                  maxDisp: Double,
                                  frontDecayFactor: Double,
                                  backDecayFactor: Double,
                                  frontDecayType: Symbol,
                                  backDecayType: Symbol,
                                  dispThreshold: Double = .0001)
extends Displacer[I, M] {
  override def displacement(implicit m: Monad[M]): VectorMap[M] = ???

  // // reciprocal of front vector modulus
  // lazy val recMaxDisp = 1 / maxDisp

  // // square of max disp
  // lazy val maxDisp2 = maxDisp * maxDisp

  // // reciprocal of square of maxDisp
  // lazy val recFrontVecMod2 = 1 / maxDisp2


  // /**
  //   * Gauss decay function - 1 for 0, monotonically falling in gaussian way.
  //   *
  //   * @param coord
  //   * @param decayFactor
  //   */
  // private def gaussDecay(coord: Double, decayFactor: Double) =
  //   exp(-.5 * coord * coord * decayFactor * decayFactor * recFrontVecMod2)

  // /**
  //   * Exponential decay function - 1 for 0, monotonically falling in exponential way.
  //   *
  //   * @param coord
  //   * @param decayFactor
  //   */
  // private def expDecay(coord: Double, decayFactor: Double) = exp(-coord * decayFactor * recMaxDisp)

  // private def linSatDecay(coord: Double, decayFactor: Double) = max(0, 1 - coord * decayFactor * recMaxDisp)

  // private def frontDecay(frontCoord: Double) = decay(frontCoord, frontDecayFactor, frontDecayType)

  // private def backDecay(backCoord: Double) = decay(backCoord, backDecayFactor, backDecayType)

  // private def decay(coord: Double, decayFactor: Double, decayType: Symbol): Double = decayType match {
  //   case DECAY_GAUSS => gaussDecay(coord, decayFactor)
  //   case DECAY_EXP => expDecay(coord, decayFactor)
  //   case DECAY_LIN_SAT => linSatDecay(coord, decayFactor)
  // }

  // /**
  //   * Distance from "to" point where displacement for given decay type and factor becomes as small as treshold value
  //   * (dispTreshold).
  //   */
  // private def treshDistance(decayType: Symbol, decayFactor: Double) = decayType match {
  //   case DECAY_GAUSS => sqrt(2 * log(frontVecMod / dispThreshold) * frontVecMod2 / (decayFactor * decayFactor))
  //   case DECAY_EXP => log(frontVecMod / dispThreshold) * frontVecMod / decayFactor
  //   case DECAY_LIN_SAT => frontVecMod / decayFactor
  // }

  // lazy val frontTreshDist = treshDistance(frontDecayType, frontDecayFactor)
  // lazy val backTreshDist = treshDistance(backDecayType, backDecayFactor)

  // override def area(implicit m: Monad[M]): ImgArea = Rect(to + (frontVersor * (frontTreshDist - backTreshDist) * .5),
  //   frontTreshDist + backTreshDist, 2 * treshDistance(sideDecayType, sideDecayFactor), frontVector.angle)

  // //  override lazy val area: ImgArea = Circle(to, .45)
  // override def displacement(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {

  //   //    override def apply(p: Point) = m.pure(Point(0, 0))

  //   override def apply(p: Point)(implicit m: Monad[M]) = m.pure({
  //     // relative vector - vector from "to" point to point "p"
  //     val relVec = p - to
  //     val main_coord = relVec * frontVersor
  //     val side_coord = abs(relVec * sideVersor)
  //     frontVector * mainDecay(main_coord)
  //   })
  // }

  // private def mainDecay(mainCoord: Double) = if (mainCoord > 0) frontDecay(mainCoord) else backDecay(-mainCoord)
}
