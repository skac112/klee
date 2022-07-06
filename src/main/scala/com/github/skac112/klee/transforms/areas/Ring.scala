package com.github.skac112.klee.transforms.areas

import cats.Monad
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgTrans, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Ring[I <: O, O, M[_]: Monad](c: Point, rLow: Double, rHigh: Double, color: I)(implicit evm: Monad[M]) extends LocalImgTrans[I, O, M] {
  override def area: ImgArea = com.github.skac112.klee.area.img.Ring(c, rLow, rHigh)
  override def applyInArea(img: Img[I, M], p: Point): M[O] = m.pure(color)

}
