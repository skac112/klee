package com.github.skac112.klee.transforms.gradients

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.{Circle, ImgArea}
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.{Angle, Point}

case class Polar[I,  M[_]: Monad](
  c: Point,
  r: Double,
  colorFun: (Double, Angle, I) => M[I],
  applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  def area: ImgArea = Circle(c, r)

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
      InstantImgPoint(ip.point, newColorM(img, ip.point), ip.land)
    } else {
      ip
  }

  def newColorM(img: Img[I, M], ptM: M[Point]) = for {
      pt <- ptM
      color <- img(pt)
      pt_c = pt - c
      new_color <- colorFun(pt_c.modulus, pt_c.angle, color)
    } yield new_color
}

