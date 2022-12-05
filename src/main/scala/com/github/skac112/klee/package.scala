package com.github.skac112

import com.github.skac112.vgutils._

import scala.math._
import java.awt.image._
import javax.imageio._
import cats._
import cats.data.Writer
import cats.implicits._
import com.github.skac112.klee.area.imgpt.AxisGrid

import scala.annotation.tailrec
import scala.collection.Seq

package object klee {
  val defWidth = 800
  type ColTrans = Color => Color
  type Values[+I] = scala.collection.Seq[I]
  type Points = Values[Point]
  type ImgPoints[I, M[_]] = Values[ImgPoint[I, M]]
  type PureImgPoints[+I] = Values[PureImgPoint[I]]
  type RealColors = scala.collection.Seq[Color]
  type ColorFun[T] = T => Color
  def trivialColorFun: ColorFun[Color] = (c: Color) => c
  def defColorFun: ColorFun[ColorVector] = (cv: ColorVector) => cv.toColor()
  def seqMonad[T, M[_]: Monad](seqM: Values[M[T]]) = seqM.toVector.sequence
  def unwrapPoints[I, M[_]: Monad](imgPoints: ImgPoints[I, M]) = seqMonad(imgPoints map { _.point })

  def drawToFileOld[I, M[_]: Monad](
                     img: Img[I, M],
                     colorFun: ColorFun[I],
                     fileName: String,
                     minX: Double,
                     maxX: Double,
                     minY: Double,
                     maxY: Double,
                     width: Int = 0,
                     height: Int = 0): M[Unit] = {
    val (dx, dy) = stepsForRender(minX, maxX, minY, maxY, width, height)
    val act_width = if (width > 0) width else floor((maxX - minX) / dx).round.toInt + 1
    val act_height = if (height > 0) height else floor((maxY - minY) / dy).round.toInt + 1
    val raster_img = new BufferedImage(act_width, act_height, BufferedImage.TYPE_INT_ARGB)
    val leftTop = Point(minX -.5*dx, minY -.5*dy)
    val pts_area = AxisGrid.forLand[I, M](img, leftTop, act_width + 1, act_height + 1, dx, dy)

    (img.applyBatchArea(pts_area) map (_ map { (ip: PureImgPoint[I]) => colorFun(ip.color) })) map { colors =>
        for (y <- 0 until act_height) {
            for (x <- 0 until act_width) {
            val shift1 = y * (act_width + 1)
            val shift2 = (y + 1) * (act_width + 1)
            // each pixel value is an average of colors of four nearest points from 'colors' sequence
            raster_img.setRGB(x, y, (colors(shift1 + x) * .25 + (colors(shift1 + x + 1) * .25) +
                (colors(shift2 + x) *.25) + (colors(shift2 + x + 1) * .25)).toInt)
            }
        }
        ImageIO.write(raster_img, "PNG", new java.io.File(fileName))    
    }
  }

  def drawToFile[M[_]: Monad](img: Img[ColorVector, M],
                              fileName: String,
                              bounds: Bounds,
                              pixelSizeX: Int,
                              pixelSizeY: Int,
                              airForceFactor: Double = 1.0,
                              colorFun: ColorFun[ColorVector] = defColorFun): M[Unit] = {
    val dx = bounds.w / pixelSizeX
    val dy = bounds.h / pixelSizeY
    val raster_img = new BufferedImage(pixelSizeX, pixelSizeY, BufferedImage.TYPE_INT_ARGB)
    val axisGridLeftTop = bounds.tl - Point(.5*dx, .5*dy)
    val pts_area = AxisGrid.forLand[ColorVector, M](img, axisGridLeftTop, pixelSizeX + 1, pixelSizeY + 1, dx, dy)
    val air_col_factor = airForceFactor / (dx * dy)

    for {
      land_colors <- img.applyBatchArea(pts_area)
      air_colors <- img.air
    } yield {
      for (y <- 0 until pixelSizeY) {
        for (x <- 0 until pixelSizeX) {
          val shift1 = y * (pixelSizeX + 1)
          val shift2 = (y + 1) * (pixelSizeX + 1)
          // each pixel value is an average of colors of four nearest points from 'colors' sequence
          val land_cv = (land_colors(shift1 + x).color  + land_colors(shift1 + x + 1).color +
            land_colors(shift2 + x).color + land_colors(shift2 + x + 1).color) * .25

          val air_cv = airColorForPixel(air_colors, bounds.tl, x, y, dx, dy)
          raster_img.setRGB(x, y, colorFun(land_cv + (air_cv * air_col_factor)).toInt)
        }
      }
      ImageIO.write(raster_img, "PNG", new java.io.File(fileName))
    }
  }

  private def pixelBounds(imgTopLeft: Point, x: Int, y: Int, dx: Double, dy: Double) = Bounds(imgTopLeft +
    Point(x * dx, y * dy), imgTopLeft + Point((x + 1) * dx, (y + 1) * dy))

  private def airColorForPixel(
                                air: PureImgPoints[ColorVector],
                                imgTopLeft: Point,
                                x: Int,
                                y: Int,
                                dx: Double,
                                dy: Double): ColorVector = (air filter { pt: PureImgPoint[ColorVector] =>
    pixelBounds(imgTopLeft, x, y, dx, dy).hitTest(pt.point) }).foldLeft (ColorVector(0, 0, 0))
    {(acc, pt) => acc + pt.color}

  private def stepsForRender(minX: Double, maxX: Double, minY: Double, maxY: Double, width: Int = 0, height: Int = 0): (Double, Double) = {
    (width, height) match {
      case (0, 0) => {
        val d = (maxX - minX) / defWidth
        (d, d)
      }
      case (0, height) => {
        val d = (maxY - minY) / height
        (d, d)
      }
      case (width, 0) => {
        val d = (maxX - minX) / width
        (d, d)
      }
      case (width, height) => {
        ((maxX - minX) / width, (maxY - minY) / height)
      }
    }
  }

  def oppositeHLColor(color: Color): Color = {
    val new_h = color.h + Pi
    val new_l = 1.0 - color.l
//    val new_l = color.l match {
//      case col if (col <= .25) => 1.0 - 2.0*col
//      case col if (col <= .5) => 2.0*col
//      case col if (col <= .75) => 2.0*col - 1.0
//      case col => -2.0*col + 2.0
//    }
    ColorVector.hsla(new_h, color.s, new_l, color.a).toColor()
  }

  def blendColors(color1: ColorVector, color2: ColorVector, proportion: Double): ColorVector = (color1 * proportion) + (color2 *
    (1.0 - proportion))

  /**
    * Returns value from normal distribution modfied by imposing minimum and maximum values.
    * @param rand
    * @param min
    * @param max
    * @param mean
    * @param stretch
    * @return
    */
  def nextGaussBounded(rand: scala.util.Random,
                       min: Double,
                       max: Double,
                       mean: Double = 0.0,
                       stretch: Double = 1.0) = math.min(max,
    math.max(min, rand.nextGaussian() * stretch + mean))

  def nextDoubleRange(rand: scala.util.Random,
                      min: Double,
                      max: Double) = {
    val d = rand.nextDouble()
    min * (1 - d) + max * d
  }
}
