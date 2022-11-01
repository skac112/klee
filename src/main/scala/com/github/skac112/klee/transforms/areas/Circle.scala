package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgPoint, ImgTrans, InstantImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Circle[I, M[_]: Monad](
                                   c: Point,
                                   r: Double,
                                   color: I,
                                   applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  override def area: ImgArea = com.github.skac112.klee.area.img.Circle(c, r)
  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
  } else {
    ip
  }

  lazy val r2 = r*r

  protected def valueM(img: Img[I, M], ptM: M[Point]): M[I] = for {
    pt <- ptM
    mod2 = (pt - c).modulus2
    value <- if (mod2 <= r2) m.pure(color) else img(pt)
  } yield value
}
