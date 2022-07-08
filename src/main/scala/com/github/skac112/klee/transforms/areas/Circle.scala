package com.github.skac112.klee.transforms.areas

import cats.Monad
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgTrans, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Circle[I <: O, O, M[_]: Monad](c: Point, r: Double, color: O) extends LocalImgTrans[I, O, M] {
  override def area: ImgArea = com.github.skac112.klee.area.img.Circle(c, r)
  override def applyInArea(img: Img[I, M], p: Point): M[O] = implicitly[Monad[M]].pure(color)
}
