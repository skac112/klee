package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.area.img.{ImgArea, Rect}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.transforms.displacers.Finger._
import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.transform.PivotRot

import scala.math._

object Finger {
  val DECAY_GAUSS = Symbol("DECAY_GAUSS")
  val DECAY_EXP = Symbol("DECAY_EXP")
  val DECAY_LIN_SAT = Symbol("DECAY_LIN_SAT")

  def maxFrontDecayFactor(from: Point, to: Point, decayType: Symbol): Double = decayType match {
    case DECAY_GAUSS => exp(.5)/(from - to).modulus
    case DECAY_EXP => 1/(from - to).modulus
    case DECAY_LIN_SAT => 1
  }
}

case class Finger[I, M[_]: Monad](from: Point,
                                  to: Point,
                                  frontDecayFactor: Double,
                                  backDecayFactor: Double,
                                  sideDecayFactor: Double,
                                  frontDecayType: Symbol,
                                  backDecayType: Symbol,
                                  sideDecayType: Symbol,
                                  dispThreshold: Double = .0001)
  extends Displacer[I, M] {
  // front vector
  lazy val frontVector = from - to

  // front vector modulus
  lazy val frontVecMod = frontVector.modulus

  // reciprocal of front vector modulus
  lazy val recFrontVecMod = 1 / frontVecMod

  // square of front vector modulus
  lazy val frontVecMod2 = frontVector.modulus2

  // reciprocal of square of front vector modulus
  lazy val recFrontVecMod2 = 1 / frontVecMod2


  // front versor
  lazy val frontVersor = frontVector.versor
  // side versor - perpendicular to front vector
  lazy val sideVersor = frontVersor.rot(PivotRot(.5 * Pi))

  /**
    * Gauss decay function - 1 for 0, monotonically falling in gaussian way.
    * @param coord
    * @param decayFactor
    */
  private def gaussDecay(coord: Double, decayFactor: Double) =
    exp(-.5 * coord * coord * decayFactor * decayFactor * recFrontVecMod2)

  /**
    * Exponential decay function - 1 for 0, monotonically falling in exponential way.
    * @param coord
    * @param decayFactor
    */
  private def expDecay(coord: Double, decayFactor: Double) = exp(-coord * decayFactor * recFrontVecMod)

  private def linSatDecay(coord: Double, decayFactor: Double) = max(0, 1 - coord * decayFactor * recFrontVecMod)

  private def frontDecay(frontCoord: Double) = decay(frontCoord, frontDecayFactor, frontDecayType)

  private def backDecay(backCoord: Double) = decay(backCoord, backDecayFactor, backDecayType)

  private def sideDecay(sideCoord: Double) = decay(sideCoord, sideDecayFactor, sideDecayType)

  private def decay(coord: Double, decayFactor: Double, decayType: Symbol): Double = decayType match {
    case DECAY_GAUSS => gaussDecay(coord, decayFactor)
    case DECAY_EXP => expDecay(coord, decayFactor)
    case DECAY_LIN_SAT => linSatDecay(coord, decayFactor)
  }

  /**
    * Distance from "to" point where displacement for given decay type and factor becomes as small as treshold value
    * (dispTreshold).
    */
  private def treshDistance(decayType: Symbol, decayFactor: Double) = decayType match {
    case DECAY_GAUSS => sqrt(2*log(frontVecMod / dispThreshold) * frontVecMod2 / (decayFactor * decayFactor))
    case DECAY_EXP => log(frontVecMod / dispThreshold) * frontVecMod / decayFactor
    case DECAY_LIN_SAT => frontVecMod / decayFactor
  }

  lazy val frontTreshDist = treshDistance(frontDecayType, frontDecayFactor)
  lazy val backTreshDist = treshDistance(backDecayType, backDecayFactor)

  println(frontTreshDist)
  println(backTreshDist)
  println(to + (frontVersor * (frontTreshDist - backTreshDist) * .5))
  println(frontTreshDist + backTreshDist)

  override lazy val area: ImgArea = Rect(to + (frontVersor * (frontTreshDist - backTreshDist) * .5),
    frontTreshDist + backTreshDist, 2*treshDistance(sideDecayType, sideDecayFactor), frontVector.angle)

  override lazy val displacement: VectorMap[M] = new VectorMap[M] {

    override def apply(p: Point) = m.pure({
      // relative vector - vector from "to" point to point "p"
      val relVec = p - to
      val main_coord = relVec * frontVersor
      val side_coord = abs(relVec * sideVersor)
      frontVector * mainDecay(main_coord) * sideDecay(side_coord)
    })
  }

  private def mainDecay(mainCoord: Double) = if (mainCoord > 0) frontDecay(mainCoord) else backDecay(-mainCoord)
}


