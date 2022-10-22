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
  def seqMonad[T, M[_]: Monad](seqM: Values[M[T]]) = seqM.toVector.sequence
  def unwrapPoints[I, M[_]: Monad](imgPoints: ImgPoints[I, M]) = seqMonad(imgPoints map { _.point })

  def drawToFile[I, M[_]: Monad](
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
}
