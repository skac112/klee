package com.github.skac112.klee.transforms

import com.github.skac112.klee.LocalImgTrans
import com.github.skac112.klee.area.img.AxisRect
import cats.Monad
import com.github.skac112.klee.{Img, ImgPoint}
import com.github.skac112.vgutils.Point
import com.github.skac112.klee.InstantImgPoint
import cats.Monad
import cats.implicits._
import scala.math._

/**
  * Emulates pixel (raster) image.
  * @tparam I
  * @tparam M
  */
trait PixelImg[I, M[_]] extends LocalImgTrans[I, M] {
  def width: Int
  def height: Int
  def pixelValue(x: Int, y: Int): M[I]
  
  override def area(implicit m: Monad[M]) = AxisRect(Point(0, 0), width, height)

  def applyInArea(img: Img[I, M], ip: ImgPoint[I, M])(implicit m: Monad[M]): ImgPoint[I, M] = 
    InstantImgPoint(ip.point, newColorM(ip.point), ip.land)

  def newColorM(ptM: M[Point])(implicit m: Monad[M]): M[I] = for {
    pt <- ptM
    pixel_val <- pixelValue(floor(pt.x).toInt, floor(pt.y).toInt)
  } yield pixel_val
}
