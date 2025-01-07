package com.github.skac112.klee.images

import cats.Monad
import cats.implicits.{*, given}
import com.github.skac112.klee.Img
import com.github.skac112.vgutils.{ColorVector, Point}

case class Raster[M[_]: Monad](pixels: Vector[Vector[ColorVector]]) extends Img[ColorVector, M]:
//  lazy val
  override def apply(p: Point)(using m: Monad[M]): M[ColorVector] = ???

  extension (number: Double)
    inline def frac: Double = number - scala.math.floor(number)
    inline def floor: Double = scala.math.floor(number)
    inline def ceil: Double = scala.math.ceil(number)

  def pixel(x: Int, y: Int): ColorVector = pixels(y)(x)
  lazy val width = pixels(0).length
  lazy val height = pixels.length

  private def bilinearInterpolation(p: Point): ColorVector =
    val (px1, py1) = (p.x.floor.toInt, p.y.floor.toInt)
    val (px2, py2) = (px1 + 1, py1 + 1)
    val (dx, dy) = (p.x - px1, p.y - py1)
    val (c11, c21) = (pixel(px1, py1), pixel(px2, py1))
    val (c12, c22) = (pixel(px1, py2), pixel(px2, py2))
    c11 * (1 - dx) * (1 - dy) + c21 * dx * (1 - dy) + c12 * (1 - dx) * dy + c22 * dx * dy

  private def bicubicInterpolation(p: Point): ColorVector =
    val px1 = math.max(if p.x.frac >= 0.5 then p.x.floor.toInt else p.x.floor.toInt - 1, 0)
    val px2 = math.min(px1 + 1, width - 1)
    val py1 = math.max(if p.y.frac >= 0.5 then p.y.floor.toInt else p.y.floor.toInt - 1, 0)
    val py2 = math.min(py1 + 1, height - 1)
    val (c11, c21) = (pixel(px1, py1), pixel(px2, py1))
    val (c12, c22) = (pixel(px1, py2), pixel(px2, py2))
    val (dx, dy) = (math.max(p.x - px1 - .5, .0), math.max(p.y - py1 - .5, .0))
    
