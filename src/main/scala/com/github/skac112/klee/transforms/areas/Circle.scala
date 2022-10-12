package com.github.skac112.klee.transforms.areas

import cats.Monad
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgTrans, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Circle[I, M[_]: Monad](c: Point, r: Double, color: I) extends LocalImgTrans[I, M] {
  override def area: ImgArea = com.github.skac112.klee.area.img.Circle(c, r)
  override def applyInArea(img: Img[I, M], p: Point): M[I] = implicitly[Monad[M]].pure(color)
}
