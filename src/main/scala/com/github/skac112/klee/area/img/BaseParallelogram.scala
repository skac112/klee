package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.transform.Affine
import com.github.skac112.vgutils.{Angle, Point}

import scala.math.*

trait BaseParallelogram extends SpriteArea {
  def middle: Point

  def xSideLen: Double

  def ySideLen: Double

  def xAngle: Angle

  def yAngle: Angle

  lazy val xVersor: Point = Point.versor(xAngle)
  lazy val yVersor: Point = Point.versor(yAngle)
  lazy val xSideHalf: Double =.5 * xSideLen
  lazy val ySideHalf: Double =.5 * ySideLen

  override def contains(p: Point): Boolean = {
    val from_mid = p - middle
    val local_x = from_mid * xVersor
    val local_y = from_mid * yVersor
    abs(local_x) <= xSideHalf && abs(local_y) <= ySideHalf
  }
  
  def transform(affine: Affine): BaseParallelogram  = {
    val new_middle = affine(middle)
    val trans_x_versor = affine(xVersor)
    val trans_y_versor = affine(yVersor)
    // długość boku x zmienia się o czynnik równy modułowi przetransformowanego wersora
    val new_x_side = xSideLen * trans_x_versor.modulus
    // długość boku y zmienia się o czynnik równy modułowi przetransformowanego wersora
    val new_y_side = ySideLen * trans_y_versor.modulus
    
    QuickParallelogram(
      affine(middle),
      xSideLen * trans_x_versor.modulus,
      ySideLen * trans_y_versor.modulus,
      trans_x_versor.angle, 
      trans_y_versor.angle)
  }
}