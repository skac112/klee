package com.github.skac112.klee.images

import cats.Monad
import com.github.skac112.klee.Img
import com.github.skac112.vgutils.{Color, ColorVector, Point}

case class Fill[M[_]](color: ColorVector)(using ev: Monad[M]) extends Img[M] {
//  override implicit val m = ev
  override def apply(p: Point)(implicit m: Monad[M]): M[ColorVector] = m.pure(color)
}
