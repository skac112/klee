package com.github.skac112.klee.images.raster

import cats.Monad
import cats.implicits.*
import com.github.skac112.klee.Img
import com.github.skac112.vgutils.{ColorVector, Point, given_Conversion_Double_Angle}

object Raster:
  enum Interpolation:
    case Nearest, Bilinear, Bicubic

import com.github.skac112.klee.images.raster.Raster.*

trait Raster[M[_]: Monad] extends Img[M] {
  def width: Int
  def height: Int
  def interpolation: Interpolation = Interpolation.Bilinear
  def pixels: Seq[Seq[M[ColorVector]]]

  protected val m = summon[Monad[M]]

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

  def pixel(x: Int, y: Int): M[ColorVector] = pixels(x)(y)

  extension(number: Double)
    inline def frac: Double = number - scala.math.floor(number)
    inline def floor: Double = scala.math.floor(number)
    inline def ceil: Double = scala.math.ceil(number)

  override def apply(p: Point)(using m: Monad[M]): M[ColorVector] = interpolation match {
    case Interpolation.Nearest => pixel(p.x.floor.toInt, p.y.floor.toInt)
    case Interpolation.Bilinear => bilinearInterpolation(p)
    case Interpolation.Bicubic => bicubicInterpolation(p)
  }

  private def bilinearInterpolation(p: Point): M[ColorVector] =
    val (px1, py1) = (p.x.floor.toInt, p.y.floor.toInt)
    val (px2, py2) = (px1 + 1, py1 + 1)
    val (dx, dy) = (p.x - px1, p.y - py1)
    val (c11m, c21m) = (pixel(px1, py1), pixel(px2, py1))
    val (c12m, c22m) = (pixel(px1, py2), pixel(px2, py2))
    for {
      c11 <- c11m
      c21 <- c21m
      c12 <- c12m
      c22 <- c22m
    } yield {
      c11 * (1 - dx) * (1 - dy) + c21 * dx * (1 - dy) + c12 * (1 - dx) * dy + c22 * dx * dy
    }

  private def bicubicInterpolation(p: Point): M[ColorVector] =
    val px = p.x.floor.toInt
    val py = p.y.floor.toInt
    val dx = p.x - px
    val dy = p.y - py

    // Pobieramy 16 pikseli w siatce 4x4 wokół punktu
    val pixelMatrix = for {
      j <- -1 to 2
      i <- -1 to 2
    } yield {
      val x = math.max(0, math.min(width - 1, px + i))
      val y = math.max(0, math.min(height - 1, py + j))
      pixel(x, y)
    }

    // Funkcje wagowe dla interpolacji bikubicznej (funkcje Catmull-Rom)
    def cubicWeight(t: Double): Array[Double] = {
      val t2 = t * t
      val t3 = t2 * t
      Array(
        -0.5 * t3 + t2 - 0.5 * t, // w-1
        1.5 * t3 - 2.5 * t2 + 1.0, // w0
        -1.5 * t3 + 2.0 * t2 + 0.5 * t, // w1
        0.5 * t3 - 0.5 * t2 // w2
      )
    }

    val wx = cubicWeight(dx)
    val wy = cubicWeight(dy)

    // Konwertujemy sekwencję M[ColorVector] na M[Seq[ColorVector]]
    pixelMatrix.toList.sequence.map { colors =>
      var result = ColorVector.hsla(0.0, 0.0, 0.0, 0.0)

      for (j <- 0 until 4; i <- 0 until 4) {
        val weight = wx(i) * wy(j)
        val colorIndex = j * 4 + i
        result = result + (colors(colorIndex) * weight)
      }

      result
    }
}
