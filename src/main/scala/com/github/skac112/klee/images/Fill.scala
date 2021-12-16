package com.github.skac112.klee.images

import com.github.skac112.klee.Img
import com.github.skac112.vgutils.{Color, Point}

case class Fill[T](color: T) extends Img[T] {
  override def apply(p: Point) = color
}
