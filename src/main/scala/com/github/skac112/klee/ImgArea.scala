package com.github.skac112.klee

import com.github.skac112.vgutils.Point

trait ImgArea {
  def contains(p: Point): Boolean
  def containedIn(other: ImgArea): Boolean = ???
  def outsideOf(other: ImgArea): Boolean = ???
}
