package com.github.skac112.klee.transforms.gradients

import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import cats.Monad
import cats.implicits._
import com.github.skac112.vgutils.Point
import com.github.skac112.klee.area.img.ImgArea

import scala.math._

case class Ring[I,  M[_]: Monad](
                                  c: Point,
                                  r: Double,
                                  areaWidth: Double,
                                  radialColorFun: (Double, I) => M[I],
                                  applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  def area: ImgArea = com.github.skac112.klee.area.img.Ring(c, max(r - .5*areaWidth, 0.0), r + .5*areaWidth)
  
  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, newColorM(img, ip.point), ip.land)
  } else {
    ip
  }

  def newColorM(img: Img[I, M], ptM: M[Point]) = for {
    pt <- ptM
    color <- img.apply(pt)
    d = abs((pt - c).modulus - r)
    new_color <- radialColorFun(d, color)
  } yield new_color


}
