package com.github.skac112.klee.transforms.gradients

import com.github.skac112.klee.LocalImgTrans
import cats.Monad
import cats.implicits._
import com.github.skac112.vgutils.Point
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.Img
import com.github.skac112.klee.area.img.Circle

case class Radial[I <: O, O,  M[_]: Monad](c: Point, r: Double, radialColorFun: (Double, I) => M[O]) extends LocalImgTrans[I, O, M] {
  def area: ImgArea = Circle(c, r)
  
  override def applyInArea(img: Img[I, M], p: Point): M[O] = for {
    color <- img(p)
    d = (p - c).modulus 
    new_color <- radialColorFun(d, color)
  } yield new_color
}
