package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.vgutils.{Angle, Point}

case class Rect[I, M[_]: Monad](middle: Point,
                                xSize: Double,
                                ySize: Double,
                                color: I,
                                xAngle: Angle = 0,
                                applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  override def area: ImgArea = com.github.skac112.klee.area.img.Rect(middle, xSize, ySize, xAngle)

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
  } else {
    ip
  }

  protected def valueM(img: Img[I, M], ptM: M[Point]): M[I] = for {
    pt <- ptM
    value <- if (area.contains(pt)) m.pure(color) else img(pt)
  } yield value
}
