package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.{Angle, Point}

case class HalfPlane[I, M[_]: Monad](
                                   // some point on a line limiting halfplane
                                   linePoint: Point,
                                   // angle of vector pointing toward a halfplane and perpendicular to limiting line
                                   normalDir: Angle,
                                   color: I,
                                   applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  override def area: ImgArea = com.github.skac112.klee.area.img.HalfPlane(linePoint, normalDir)
  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
  } else {
    ip
  }

  protected def valueM(img: Img[I, M], ptM: M[Point]): M[I] = m.pure(color)

//  protected def valueM(img: Img[I, M], ptM: M[Point]): M[I] = for {
//    pt <- ptM
//    value <- if ((pt - linePoint) * normalDir.versor >= 0) m.pure(color) else img(pt)
//  } yield value
}
