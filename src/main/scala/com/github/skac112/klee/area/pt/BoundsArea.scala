package com.github.skac112.klee.area.pt

import com.github.skac112.klee.Points
import com.github.skac112.vgutils.Bounds

object BoundsArea {
  def forPts(pts: Points) = BoundsArea(Bounds.forPts(pts.toSet), pts)
}

case class BoundsArea(givenBounds: Bounds, override val points: Points) extends PtArea {
  override def area = com.github.skac112.klee.area.img.BoundsArea(givenBounds)
}
