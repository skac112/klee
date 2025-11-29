package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits.*
import com.github.skac112.klee.area.img.{ImgArea, Ring => RingArea}
import com.github.skac112.klee.{Img, ImgPoint, ImgTrans, InstantImgPoint, LandImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.{Color, ColorVector, Point}

case class Ring[M[_]](
                                 c: Point,
                                 rLow: Double,
                                 rHigh: Double,
                                 color: ColorVector,
                                 applyToAir: Boolean = true) extends LocalImgTrans[M] {
  lazy val rLow2 = rLow*rLow
  lazy val rHigh2 = rHigh*rHigh
  override def area: ImgArea = RingArea(c, rLow, rHigh)
//  override def applyInArea(img: Img[M], p: Point): M[O] = implicitly[Monad[M]].pure(color)
  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = if (applyToAir || ip.land) {
      InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
    } else {
    ip
  }

  protected def valueM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]): M[ColorVector] = for {
    pt <- ptM
    mod2 = (pt - c).modulus2
    value <- if (mod2 >= rLow2 && mod2 <= rHigh2) m.pure(color) else img(pt)
  } yield value
}
