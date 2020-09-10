package com.github.skac112.klee.dynsys.vectormaps

import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.transform.Linear

object VectorMap {
  def from(fun: Point => Point) = new VectorMap {
    override def apply(p: Point) = fun(p)
  }

  def identity = new VectorMap {
    override def apply(p: Point) = p
  }
}

trait VectorMap extends (Point => Point) {
  lazy val f1_6 = 1.0 / 6

  /**
    * Transforms this vector map to a new vector map making one step of integration using given
    * vector map as a velocity field.
    *
    * @param h
    */
  def rungeKutta4(motionEq: VectorMap, h: Double): VectorMap = {
    // caution: all operations in the body of method are performed on vectormaps (functions) rather than on a
    // points or numbers (implicitly k1 = motionEq).
    val k2 = motionEq compose (identity + (motionEq * (.5 * h)))
    val k3 = motionEq compose (identity + (k2 * (.5 * h)))
    val k4 = motionEq compose (identity + (k3 * h))
    this + (motionEq + (k2 * 2) + (k3 * 2) + k4) * h * f1_6
  }

  def identity: VectorMap = (p: Point) => p

  def *(factor: Double): VectorMap = (p: Point) => apply(p) * factor

  def /(factor: Double): VectorMap = (p: Point) => apply(p) / factor

  def +(otherPt: Point): VectorMap = (p: Point) => apply(p) + otherPt

  def -(otherPt: Point): VectorMap = (p: Point) => apply(p) - otherPt

  def +(other: VectorMap): VectorMap = (p: Point) => this.apply(p) + other(p)

  def -(other: VectorMap): VectorMap = (p: Point) => this.apply(p) - other(p)

  def jacobi(p: Point): Linear = ???

  /**
    * This implementation is analogous to and hides base class (Function2) implementation.
    * @param other
    * @return
    */
  def compose(other: VectorMap): VectorMap = (p: Point) => this.apply(other(p))

  /**
    * This implementation is analogous to and hides base class (Function2) implementation.
    * @param other
    * @return
    */
  def andThen(other: VectorMap): VectorMap = (p: Point) => other(this.apply(p))
}
