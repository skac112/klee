package com.github.skac112

import com.github.skac112.vgutils._

import scala.math._
import java.awt.image._
import javax.imageio._

import cats.{Monad, Monoid}
import cats.data.Writer
import cats.implicits._

import scala.annotation.tailrec
import scala.collection.Seq

package object klee {
  val defWidth = 800
//  type Img = Point => Color
  type ColTrans = Color => Color
//  type DrawingMaker[A] = cats.data.Writer[ImgTrans, A]
//  def identity[T] = (img: Img[T]) => img
  type Points = Seq[Point]
  type Colors = Seq[Color]
  type ColorFun[T] = T => Color
  def trivialColorFun: ColorFun[Color] = (c: Color) => c

  def drawToFile[I, M[_]: Monad](
                     imgFun: Img[I, M],
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
    val act_height = if (height > 0) height else floor((maxY - minY) / dx).round.toInt + 1
    val img = new BufferedImage(act_width, act_height, BufferedImage.TYPE_INT_ARGB)

    val points = for {
      x <- 0 to act_width
      y <- 0 to act_height
    } yield Point(minX + dx * x, minY + dy * y)

    for {
      colors <- imgFun.applyBatch(points) map (_ map colorFun)
      _ = for (x <- 0 until act_width) {
        for (y <- 0 until act_height) {
          val shift1 = x * (act_height + 1)
          val shift2 = (x + 1) * (act_height + 1)
          // each pixel value is an average of colors of four nearest points from 'colors' sequence
          img.setRGB(x, y, math.round(.25 * (colors(shift1 + y).toInt + colors(shift1 + y + 1).toInt +
            colors(shift2 + y).toInt + colors(shift2 + y + 1).toInt)).toInt)
        }
      }
      _ = ImageIO.write(img, "PNG", new java.io.File(fileName))
      // yield-ed value is unused, .toList is used heree just to make compiler shut up :)
    } yield colors.toList
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

//  implicit val drawingMakerMonadInstance: Monad[DrawingMaker] = new Monad[DrawingMaker] {
//    def pure[A](a: A): DrawingMaker[A] = Writer(Nil, a)
//    def flatMap[A, B](fa: DrawingMaker[A])(f: A => DrawingMaker[B]): DrawingMaker[B] = {
//      val new_writer = f(fa.value)
//      Writer(fa.written ++ new_writer.written, new_writer.value)
//    }
//    override def map[A, B](fa: DrawingMaker[A])(f: A => B): DrawingMaker[B] = Writer(fa.written, f(fa.value))
//    @tailrec
//    def tailRecM[A, B](a: A)(f: A => DrawingMaker[Either[A, B]]): DrawingMaker[B] = {
//      val new_w = f(a)
//      val either_val = new_w.value
//      either_val match {
//        case Right(v) => Writer(new_w.written, v)
//        case Left(v) => tailRecM(v)(f)
//      }
//    }
//  }
//
//  implicit val ensembleMonoidInstance: Monoid[ImgTrans] = new Monoid[ImgTrans] {
//    override def combine(ens1: Ensemble, ens2: Ensemble) = ens1 ++ ens2
//    override def empty = Seq.empty[PosGraphic[Graphic]]
//  }
}
