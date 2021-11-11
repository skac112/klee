package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.Point

case class EmptyArea() extends ImgArea {
  override def contains(p: Point) = false
}
