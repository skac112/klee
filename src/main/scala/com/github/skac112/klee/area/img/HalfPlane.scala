package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.{Angle, Bounds, Point}

case class HalfPlane( // some point on a line limiting halfplane
                      linePoint: Point,
                      // angle off vector pointing toward a halfplane and perpendicular to limiting line)
                      normalDir: Angle) extends ImgArea {
  lazy val normalVersor = normalDir.versor
  override def contains(p: Point): Boolean = (p - linePoint) * normalVersor >= 0

//  override lazy val bounds: Option[Bounds] = Some(Bounds.inf)
}
