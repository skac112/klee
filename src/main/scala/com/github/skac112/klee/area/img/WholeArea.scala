package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.Point

case class WholeArea() extends ImgArea {
  override def contains(p: Point) = true
}
