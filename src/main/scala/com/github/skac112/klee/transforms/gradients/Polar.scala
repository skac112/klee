package com.github.skac112.klee.transforms.gradients

import cats.Monad
import cats.implicits.*
import com.github.skac112.klee.area.img.{Circle, ImgArea}
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.{Angle, ColorVector, Point}

/**
  * Transformation operates in a circle area. "Color" (i. e. value of an I type) in this circle depends on a current
  * color and modulus and angle of vector from center of circle to given point.
  * @param c
  * @param r
  * @param colorFun
  * @param applyToAir
  * @param monad$M$0
  * @tparam I
  * @tparam M
  */
case class Polar[ M[_]](c: Point, 
                        r: Double,
                        colorFun: (Double, Angle, ColorVector) => M[ColorVector],
                        applyToAir: Boolean = false) extends LocalImgTrans[M] {
  def area(implicit m: Monad[M]): ImgArea = Circle(c, r)

  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = if (applyToAir || ip.land) {
      InstantImgPoint(ip.point, newColorM(img, ip.point), ip.land)
    } else {
      ip
  }

  def newColorM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]) = for {
      pt <- ptM
      color <- img(pt)
      pt_c = pt - c
      new_color <- colorFun(pt_c.modulus, pt_c.angle, color)
    } yield new_color
}

