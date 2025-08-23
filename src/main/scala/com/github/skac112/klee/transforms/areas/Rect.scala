package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits.*
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.vgutils.{Angle, ColorVector, Point, given}

case class Rect[M[_]](middle: Point,
                                xSize: Double,
                                ySize: Double,
                                color: ColorVector,
                                xAngle: Angle = .0,
                                applyToAir: Boolean = false) extends LocalImgTrans[M] {
  override def area(implicit m: Monad[M]): ImgArea = com.github.skac112.klee.area.img.Rect(middle, xSize, ySize, xAngle)

  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
  } else {
    ip
  }

  protected def valueM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]): M[ColorVector] = for {
    pt <- ptM
    value <- if (area.contains(pt)) m.pure(color) else img(pt)
  } yield value
}
