package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.area.img.{Circle, ImgArea, Rect}
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

/**
  * Displacer emulating point-to-point local translation of part of image. One can imagine image made of some elastic
  * material. Then one can touch an image with finger (hence the name) in "from" point and move it to the "to" point.
  * Due to size of "finger" (determined by decay factors and type) some neighbourhood of "from" point is moved
  * accordingly. Due to method of how every displacer work, a basic displacement is defined for "to" point and is a
  * vector from "to" point to "from" point. This is the maximum displacement. Notions "front", "back" and "side"
  * relate to point to and vector of maximum displacement. Line connecting "to" and "from" point displacement
  * is divided into two rays staring in "to" point. One ray goes through "from" point - it is the front ray, the other
  * ray is back ray. Displacement for points on front ray decays according to front decay starting from "to" point.
  * Accordingly, displacement for point on back ray decay according to back decay starting from "to" point. Front and
  * back decay define together main decay. For points outside line connecting points "from" and "two" (main decay line)
  * a side decay is needed to calculate total decay and then displacement. Side decay is calculated from distance of
  * given point from main decay line. Total decay is calculated as a multiplication of main decay and side decay.
  * Front decay has upper limit which being exceeded, gives a no one-to-one function. It is so because there is a
  * "squeezing" on a main ray. A "singularity" is created when, moving from "to" point on a front ray a displacement
  * decays faster than distance on a ray.
  * @param from
  * @param to
  * @param frontDecayFactor
  * @param backDecayFactor
  * @param sideDecayFactor
  * @param frontDecayType
  * @param backDecayType
  * @param sideDecayType
  * @param dispThreshold
  * @tparam I
  * @tparam M
  */
case class Finger[M[_]](from: Point,
                           to: Point,
                           frontDecayFactor: Double,
                           backDecayFactor: Double,
                           sideDecayFactor: Double,
                           frontDecayType: Symbol,
                           backDecayType: Symbol,
                           sideDecayType: Symbol,
                           dispThreshold: Double = .0001)
  extends Displacer[M] {
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

  override def area: ImgArea = Rect(to + (frontVersor * (frontTreshDist - backTreshDist) * .5),
    frontTreshDist + backTreshDist, 2*treshDistance(sideDecayType, sideDecayFactor), frontVector.angle)

//  override lazy val area: ImgArea = Circle(to, .45)
  override def displacement(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {

//    override def apply(p: Point) = m.pure(Point(0, 0))

    override def apply(p: Point)(implicit m: Monad[M]) = m.pure({
      // relative vector - vector from "to" point to point "p"
      val relVec = p - to
      val main_coord = relVec * frontVersor
      val side_coord = abs(relVec * sideVersor)
      frontVector * mainDecay(main_coord) * sideDecay(side_coord)
    })
  }

  private def mainDecay(mainCoord: Double) = if (mainCoord > 0) frontDecay(mainCoord) else backDecay(-mainCoord)
}


