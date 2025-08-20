package com.github.skac112.klee.transforms.gradients

import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import cats.Monad
import cats.implicits._
import com.github.skac112.vgutils.Point
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.img.Circle

/**
  * Transformation operates in a circle area. "Color" (i. e. value of an I type) of point in this circle depends on
  * a current color and modulus of vector from center of circle to given point.
  * @param c
  * @param r
  * @param radialColorFun
  * @param applyToAir
  * @param monad$M$0
  * @tparam I
  * @tparam M
  */
case class Radial[ M[_]](
                                    c: Point,
                                    r: Double,
                                    radialColorFun: (Double, I) => M,
                                    applyToAir: Boolean = false) extends LocalImgTrans[M] {
  override def area(implicit m: Monad[M]): ImgArea = Circle(c, r)

  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, newColorM(img, ip.point), ip.land)
  } else {
    ip
  }

  def newColorM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]) = for {
    pt <- ptM
    color <- img.apply(pt)
    d = (pt - c).modulus
    new_color <- radialColorFun(d, color)
  } yield new_color
}
