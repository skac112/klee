package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits.*
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgPoint, ImgTrans, InstantImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.{Color, ColorVector, Point}

case class Circle[M[_]](
                                   c: Point,
                                   r: Double,
                                   color: ColorVector,
                                   applyToAir: Boolean = false) extends LocalImgTrans[M] {
  override def area: ImgArea = com.github.skac112.klee.area.img.Circle(c, r)
  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
  } else {
    ip
  }

  lazy val r2 = r*r

  protected def valueM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]): M[ColorVector] = for {
    pt <- ptM
    mod2 = (pt - c).modulus2
    value <- if (mod2 <= r2) m.pure(color) else img(pt)
  } yield value
}
