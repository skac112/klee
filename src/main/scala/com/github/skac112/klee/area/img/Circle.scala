package com.github.skac112.klee.area.img
import com.github.skac112.vgutils.{Angle, Bounds, Point}

case class Circle(c: Point, r: Double) extends ImgArea {
  lazy val r2 = r*r
  override def contains(p: Point): Boolean = (p - c).modulus2 <= r2

  override lazy val bounds: Option[Bounds] = {
    lazy val vec = Point(r, r)
    Some(Bounds(c - vec, c + vec))
  }
}
