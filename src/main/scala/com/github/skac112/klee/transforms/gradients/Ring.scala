package com.github.skac112.klee.transforms.gradients

import com.github.skac112.klee.LocalImgTrans
import cats.Monad
import cats.implicits._
import com.github.skac112.vgutils.Point
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.Img
import scala.math._

case class Ring[I <: O, O,  M[_]: Monad](c: Point, r: Double, areaWidth: Double, radialColorFun: (Double, I) => M[O]) extends LocalImgTrans[I, O, M] {
  def area: ImgArea = com.github.skac112.klee.area.img.Ring(c, max(r - .5*areaWidth, 0.0), r + .5*areaWidth)
  
  override def applyInArea(img: Img[I, M], p: Point): M[O] = for {
    color <- img(p)
    d = abs((p - c).modulus - r) 
    new_color <- radialColorFun(d, color)
  } yield new_color
}
