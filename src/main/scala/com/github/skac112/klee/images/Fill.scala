package com.github.skac112.klee.images

import cats.Monad
import com.github.skac112.klee.Img
import com.github.skac112.vgutils.{Color, Point}

case class Fill[I, M[_]: Monad](color: I)(implicit ev: Monad[M]) extends Img[I, M] {
  override implicit val m = ev
  override def apply(p: Point) = m.pure(color)
}
