package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.{Angle, Point}

case class HalfPlane[M[_]](
                                   // some point on a line limiting halfplane
                                   linePoint: Point,
                                   // angle of vector pointing toward a halfplane and perpendicular to limiting line
                                   normalDir: Angle,
                                   color: I,
                                   applyToAir: Boolean = false) extends LocalImgTrans[M] {
  override def area(implicit m: Monad[M]): ImgArea = com.github.skac112.klee.area.img.HalfPlane(linePoint, normalDir)
  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
  } else {
    ip
  }

  protected def valueM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]): M = m.pure(color)

//  protected def valueM(img: Img[M], ptM: M[Point]): M = for {
//    pt <- ptM
//    value <- if ((pt - linePoint) * normalDir.versor >= 0) m.pure(color) else img(pt)
//  } yield value
}
