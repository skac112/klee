package com.github.skac112.klee.transforms.gradients

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.klee.area.img.{Circle, ImgArea}
import com.github.skac112.vgutils.{Angle, Point}

/**
  * Transformation operates in a circle area. "Color" (i. e. value of an I type) in this circle depends on a current
  * color and angle of vector from center of circle to given point.
  * @param c
  * @param r
  * @param angleColorFun
  * @param applyToAir
  * @param monad$M$0
  * @tparam I
  * @tparam M
  */
case class Angular[I,  M[_]](
                                     c: Point,
                                     r: Double,
                                     angleColorFun: (Angle, I) => M[I],
                                     applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  def area(implicit m: Monad[M]): ImgArea = Circle(c, r)

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M])(implicit m: Monad[M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
      InstantImgPoint(ip.point, newColorM(img, ip.point), ip.land)
    } else {
      ip
  }

  def newColorM(img: Img[I, M], ptM: M[Point])(implicit m: Monad[M]) = for {
      pt <- ptM
      color <- img.apply(pt)
      angle = (pt - c).angle
      new_color <- angleColorFun(angle, color)
    } yield new_color
}

