package com.github.skac112.klee

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.vgutils.Point

/**
  * Colorizer - value of a point (it's "color" - though exact type is not necesarilly a color) depends only on a "color"
  * of a point in input image, so the function in it's application area is wholly determined by the colorFun.
  */
abstract class Colorizer[I,  M[_]: Monad] extends LocalImgTrans[I, M] {
  def area: ImgArea = WholeArea()
  def colorFun: (I) => M[I]
  def applyToAir: Boolean = true

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]) = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, newColorM(img, ip.point), ip.land)
  } else {
    ip
  }

  protected def newColorM(img: Img[I, M], ptM: M[Point]): M[I] = for {
    pt <- ptM
    color <- img(pt)
    new_color <- colorFun(color)
  } yield new_color
}
