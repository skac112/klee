package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.{Bounds, Point}

case class BoundsArea(givenBounds: Bounds) extends ImgArea{
  override def contains(p: Point) = givenBounds.hitTest(p)
  override def bounds = Some(givenBounds)
}
