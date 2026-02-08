package com.github.skac112.klee.images.raster

import cats.implicits.{*, given}
import cats.{Id, Monad}
import Raster.*
import com.github.skac112.klee.*
import com.github.skac112.vgutils.{ColorVector, Point}

case class MutableRaster[M[_]: Monad](
                                       override val width: Int,
                                       override val height: Int,
                                       override val interpolation: Interpolation = Interpolation.Bilinear,
                                       initImg: Img[M])
  extends Raster[M]:

  def updatePixel(x: Int, y: Int, color: ColorVector)(using m: Monad[M]): MutableRaster[M] = {
    pixelArr(x)(y) = m.pure(color)
    this
  }

  private lazy val pixelArr: Array[Array[M[ColorVector]]] = {
    val arr = new Array[Array[M[ColorVector]]](width)
    for (x <- 0 until width) {
      arr(x) = new Array[M[ColorVector]](height)
      for (y <- 0 until height)
        arr(x)(y) = initImg(Point(x, y))
    }
    arr
  }

  override def pixels: Seq[Seq[M[ColorVector]]] = pixelArr.map(_.toSeq).toSeq

  def imgPoints: ImgPoints[M] = for {
      y <- 0 until height
      x <- 0 until width
      pt = Point(x, y)
      color = pixelArr(x)(y)
  } yield InstantImgPoint(m.pure(Point(x, y)), color)
