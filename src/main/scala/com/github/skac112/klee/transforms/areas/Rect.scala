package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.vgutils.{Angle, Point}
import com.github.skac112.vgutils.given

case class Rect[I, M[_]](middle: Point,
                                xSize: Double,
                                ySize: Double,
                                color: I,
                                xAngle: Angle = .0,
                                applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  override def area(implicit m: Monad[M]): ImgArea = com.github.skac112.klee.area.img.Rect(middle, xSize, ySize, xAngle)

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M])(implicit m: Monad[M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
  } else {
    ip
  }

  protected def valueM(img: Img[I, M], ptM: M[Point])(implicit m: Monad[M]): M[I] = for {
    pt <- ptM
    value <- if (area.contains(pt)) m.pure(color) else img(pt)
  } yield value
}
