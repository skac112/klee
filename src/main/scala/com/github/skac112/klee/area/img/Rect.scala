package com.github.skac112.klee.area.img

import breeze.numerics.abs
import com.github.skac112.vgutils.*
import com.github.skac112.vgutils.given
import com.github.skac112.vgutils.transform.{Affine, PivotRot}

import scala.math.Pi

case class Rect(middle: Point, xSideLen: Double, ySideLen: Double, xAngle: Angle = 0.0) extends BaseParallelogram {
  override lazy val xVersor = Point.versor(xAngle)
  override lazy val yVersor = Point.versor(xAngle + .5*Pi)
  override lazy val xSideHalf = .5 * xSideLen
  override lazy val ySideHalf = .5 * ySideLen
  override lazy val yAngle: Angle = .0

  override def contains(p: Point): Boolean = {
    val from_mid = p - middle
    val local_x = from_mid * xVersor
    val local_y = from_mid * yVersor
    abs(local_x) <= xSideHalf && abs(local_y) <= ySideHalf
  }
}
