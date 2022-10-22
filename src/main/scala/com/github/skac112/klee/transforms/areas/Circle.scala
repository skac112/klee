package com.github.skac112.klee.transforms.areas

import cats.Monad
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgPoint, ImgTrans, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Circle[I, M[_]: Monad](
                                   c: Point,
                                   r: Double,
                                   color: I,
                                   applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  override def area: ImgArea = com.github.skac112.klee.area.img.Circle(c, r)
  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]): ImgPoint[I, M] = ???
}
