package com.github.skac112.klee

import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.transform.Linear

trait PointMap extends (Point => Point) {

  lazy val f1_6 = 1 / 6

  /**
    * Calculates new PointMap (not just a point) using point map of this object as an integrand.
    *
    * @param h
    */
  def rungeKutta4(h: Double): PointMap = {
      // caution: all operations in the body of method are performed on pointmaps (functions) rather than on a
      // points or numbers (implicitly k1 = this).
      val k2 = this compose (identity + (this * (.5 * h)))
      val k3 = this compose (identity + (k2 * (.5 * h)))
      val k4 = (this compose (identity + (k3 * h)))
      identity + (this + (k2 * 2) + (k3 * 2) + k4) * f1_6
    }

  def identity: PointMap = (p: Point) => p

  def *(factor: Double): PointMap = (p: Point) => p * factor

  def +(otherPt: Point): PointMap = (p: Point) => p + otherPt

  def +(other: PointMap): PointMap = (p: Point) => this(p) + other(p)

  def jacobi(p: Point): Linear = ???

  /**
    * This implementation is analogous to and hides base class (Function2) implementation.
    * @param other
    * @return
    */
  def compose(other: PointMap): PointMap = (p: Point) => this(other(p))

  /**
    * This implementation is analogous to and hides base class (Function2) implementation.
    * @param other
    * @return
    */
  def andThen(other: PointMap): PointMap = (p: Point) => other(this(p))
}
