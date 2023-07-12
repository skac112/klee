package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.Bounds

case class MultiPartArea(parts: Set[ImgArea]) extends ImgArea {
  override def contains(p: Point): Boolean = parts exists { part => part.contains(p) }

  override lazy val bounds = Some(parts.foldLeft(Bounds.empty) {(curr_bounds, part) => curr_bounds + part.bounds.getOrElse(Bounds.inf)})
}
