package com.github.skac112.klee.images

import cats.Monad
import com.github.skac112.klee.Img
import com.github.skac112.vgutils.{Color, Point}

case class Fill[T, M[_]: Monad](color: T)(implicit ev: Monad[M]) extends Img[T, M] {
  override implicit val m = ev
  override def apply(p: Point) = m.pure(color)
}
