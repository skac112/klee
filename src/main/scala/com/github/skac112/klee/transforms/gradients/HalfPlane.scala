package com.github.skac112.klee.transforms.gradients

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.vgutils.{Angle, Point}

case class HalfPlane[ M[_]](
                                       // some point on a line limiting halfplane
                                       linePoint: Point,
                                       // angle of vector pointing toward a halfplane and perpendicular to limiting line
                                       normalDir: Angle,
                                       colorFun: (Double, I) => M,
                                       applyToAir: Boolean = false) extends LocalImgTrans[M] {

  lazy val normalVersor = normalDir.versor

  override def area(implicit m: Monad[M]): ImgArea = com.github.skac112.klee.area.img.HalfPlane(linePoint, normalDir)

  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, newColorM(img, ip.point), ip.land)
  } else {
    ip
  }

  def newColorM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]) = for {
    pt <- ptM
    color <- img.apply(pt)
    d = (pt - linePoint) * normalVersor
    new_color <- colorFun(d, color)
  } yield new_color
}