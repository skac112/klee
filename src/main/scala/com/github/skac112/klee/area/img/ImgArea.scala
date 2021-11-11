package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.{Bounds, Point}

trait ImgArea {
  def contains(p: Point): Boolean

  def containedIn(other: ImgArea): Option[Boolean] = None

  def outsideOf(other: ImgArea): Option[Boolean] = None

  def intersection(other: ImgArea): Option[ImgArea] = None

  def bounds: Option[Bounds] = None
}
