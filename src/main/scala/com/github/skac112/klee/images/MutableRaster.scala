package com.github.skac112.klee.images

import cats.{Id, Monad}
import cats.implicits.{*, given}
import com.github.skac112.klee.Img
import com.github.skac112.vgutils.{*, given}

object MutableRaster:
  enum Interpolation:
    case Nearest, Bilinear, Bicubic

import MutableRaster.*

case class MutableRaster[M[_]: Monad](
                                       width: Int,
                                       height: Int,
                                       initImg: Img[ColorVector, M],
                                       interpolation: Interpolation = Interpolation.Bilinear)
  extends Img[ColorVector, M]:

  override def apply(p: Point)(using m: Monad[M]): M[ColorVector] = interpolation match {
    case Interpolation.Nearest => pixel(p.x.floor.toInt, p.y.floor.toInt)
    case Interpolation.Bilinear => bilinearInterpolation(p)
    case Interpolation.Bicubic => bicubicInterpolation(p)
  }

  private lazy val pixels: Array[M[ColorVector]] = {
    val arr = new scala.Array[M[ColorVector]](width * height)
    for (y <- 0 until height) {
      for (x <- 0 until width) {
        arr(y * width + x) = initImg(Point(x, y))
      }
    }
    arr
  }

  /**
    * Matrix for bicubic interpolation.
    */
  lazy val bcubmtx: Seq[Seq[Double]] = Vector(
    Vector(1, -1, 1, -1, -1, 1, -1, 1, 1, -1, 1, -1, -1, 1, -1, 1),
    Vector(1, 0, 0, 0, -1, 0, 0, 0, 1, 0, 0, 0, -1, 0, 0, 0),
    Vector(1, 1, 1, 1, -1, -1, -1, -1, 1, 1, 1, 1, -1, -1, -1, -1),
    Vector(1, 2, 4, 8, -1, -2, -4, -8, 1, 2, 4, 8, -1, -2, -4, -8),
    Vector(1, -1, 1, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector(1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector(1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector(1, 2, 4, 8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    Vector(1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1, 1, -1),
    Vector(1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0),
    Vector(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
    Vector(1, 2, 4, 8, 1, 2, 4, 8, 1, 2, 4, 8, 1, 2, 4, 8),
    Vector(1, -1, 1, -1, 2, -2, 2, -2, 4, -4, 4, -4, 8, -8, 8, -8),
    Vector(1, 0, 0, 0, 2, 0, 0, 0, 4, 0, 0, 0, 8, 0, 0, 0),
    Vector(1, 1, 1, 1, 2, 2, 2, 2, 4, 4, 4, 4, 8, 8, 8, 8),
    Vector(1, 2, 4, 8, 2, 4, 8, 16, 4, 8, 16, 32, 8, 16, 32, 64))

  extension (number: Double)
    inline def frac: Double = number - scala.math.floor(number)
    inline def floor: Double = scala.math.floor(number)
    inline def ceil: Double = scala.math.ceil(number)

  def pixel(x: Int, y: Int): M[ColorVector] = pixels(y * width + x)

  private def bilinearInterpolation(p: Point): M[ColorVector] =
    val (px1, py1) = (p.x.floor.toInt, p.y.floor.toInt)
    val (px2, py2) = (px1 + 1, py1 + 1)
    val (dx, dy) = (p.x - px1, p.y - py1)
    val (c11, c21) = (pixel(px1, py1), pixel(px2, py1))
    val (c12, c22) = (pixel(px1, py2), pixel(px2, py2))
    c11 * (1 - dx) * (1 - dy) + c21 * dx * (1 - dy) + c12 * (1 - dx) * dy + c22 * dx * dy

  private def bicubicInterpolation(p: Point): M[ColorVector] =
    val px1 = math.max(if p.x.frac >= 0.5 then p.x.floor.toInt else p.x.floor.toInt - 1, 0)
    val px2 = math.min(px1 + 1, width - 1)
    val py1 = math.max(if p.y.frac >= 0.5 then p.y.floor.toInt else p.y.floor.toInt - 1, 0)
    val py2 = math.min(py1 + 1, height - 1)
    val (c11, c21) = (pixel(px1, py1), pixel(px2, py1))
    val (c12, c22) = (pixel(px1, py2), pixel(px2, py2))
    val (dx, dy) = (math.max(p.x - px1 - .5, .0), math.max(p.y - py1 - .5, .0))
    // dummy temporary result
    ColorVector.hsla(.0, .0, .0, .0)
