package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.{Bounds, Point}

case class Ring(c: Point, rLow: Double, rHigh: Double) extends ImgArea {
  lazy val rLow2 = rLow * rLow
  lazy val rHigh2 = rHigh * rHigh

  override def contains(p: Point): Boolean = {
    val mod2 = (p - c).modulus2
    mod2 >= rLow2 && mod2 <= rHigh2
  }

  override lazy val bounds: Option[Bounds] = {
    lazy val vec = Point(rHigh, rHigh)
    Some(Bounds(c - vec, c + vec))
  }
}
