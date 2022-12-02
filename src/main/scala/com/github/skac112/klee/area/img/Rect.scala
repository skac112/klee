package com.github.skac112.klee.area.img

import breeze.numerics.abs
import com.github.skac112.vgutils._
import com.github.skac112.vgutils.transform.PivotRot

import scala.math.Pi

case class Rect(middle: Point, xSide: Double, ySide: Double, xAngle: Angle = 0.0) extends ImgArea {
  lazy val xVersor = Point.versor(xAngle)
  lazy val yVersor = Point.versor(xAngle + .5*Pi)
  lazy val xSideHalf = .5 * xSide
  lazy val ySideHalf = .5 * ySide

  override def contains(p: Point): Boolean = {
    val from_mid = p - middle
    val local_x = from_mid * xVersor
    val local_y = from_mid * yVersor
    abs(local_x) <= xSideHalf && abs(local_y) <= ySideHalf
  }
}
