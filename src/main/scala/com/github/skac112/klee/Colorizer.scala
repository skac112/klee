package com.github.skac112.klee

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.vgutils.Point

/**
  * Colorizer - value of a point (it's "color" - though exact type is not necesarilly a color) depends only on a "color"
  * of a point in input image, so the function in it's application area is wholly determined by the colorFun.
  */
abstract class Colorizer[M[_]] extends LocalImgTrans[M] {
  override def area(implicit m: Monad[M]): ImgArea = WholeArea()
  def colorFun: ColorVector => M[ColorVector]
  def applyToAir: Boolean = true

  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]) = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, newColorM(img, ip.point), ip.land)
  } else {
    ip
  }

  protected def newColorM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]): M = for {
    pt <- ptM
    color <- img(pt)
    new_color <- colorFun(color)
  } yield new_color
}
