package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgPoint, ImgTrans, InstantImgPoint, LandImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Ring[I, M[_]](
                                 c: Point,
                                 rLow: Double,
                                 rHigh: Double,
                                 color: I,
                                 applyToAir: Boolean = true) extends LocalImgTrans[I, M] {
  lazy val rLow2 = rLow*rLow
  lazy val rHigh2 = rHigh*rHigh
  override def area(implicit m: Monad[M]): ImgArea = com.github.skac112.klee.area.img.Ring(c, rLow, rHigh)
//  override def applyInArea(img: Img[I, M], p: Point): M[O] = implicitly[Monad[M]].pure(color)
  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M])(implicit m: Monad[M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
      InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
    } else {
    ip
  }

  protected def valueM(img: Img[I, M], ptM: M[Point])(implicit m: Monad[M]): M[I] = for {
    pt <- ptM
    mod2 = (pt - c).modulus2
    value <- if (mod2 >= rLow2 && mod2 <= rHigh2) m.pure(color) else img(pt)
  } yield value
}
