package com.github.skac112.klee.images

import com.github.skac112.klee.Img
import com.github.skac112.vgutils.{Color, Point}

case class Fill(color: Color) extends Img {
  override def apply(p: Point) = color
}
