package com.github.skac112.klee.transforms.gradients

import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import cats.Monad
import cats.implicits._
import com.github.skac112.vgutils.Point
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.img.Circle

case class Radial[I,  M[_]: Monad](
                                    c: Point,
                                    r: Double,
                                    radialColorFun: (Double, I) => M[I],
                                    applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  def area: ImgArea = Circle(c, r)

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, newColorM(img, ip.point), ip.land)
  } else {
    ip
  }

  def newColorM(img: Img[I, M], ptM: M[Point]) = for {
    pt <- ptM
    color <- img.apply(pt)
    d = (pt - c).modulus
    new_color <- radialColorFun(d, color)
  } yield new_color
}
