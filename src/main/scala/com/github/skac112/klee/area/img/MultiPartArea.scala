package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.Point

case class MultiPartArea(parts: Set[ImgArea]) extends ImgArea {
  def contains(p: Point): Boolean = parts exists { part => part.contains(p) }
}
