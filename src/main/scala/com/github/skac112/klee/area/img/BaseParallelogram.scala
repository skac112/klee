package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.{Angle, Point}
import scala.math.*

trait BaseParallelogram extends ImgArea {
  def middle: Point

  def xSide: Double

  def ySide: Double

  def xAngle: Angle

  def yAngle: Angle

  lazy val xVersor: Point = Point.versor(xAngle)
  lazy val yVersor: Point = Point.versor(yAngle)
  lazy val xSideHalf: Double =.5 * xSide
  lazy val ySideHalf: Double =.5 * ySide

  override def contains(p: Point): Boolean = {
    val from_mid = p - middle
    val local_x = from_mid * xVersor
    val local_y = from_mid * yVersor
    abs(local_x) <= xSideHalf && abs(local_y) <= ySideHalf
  }
}