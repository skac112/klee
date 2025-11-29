package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.Bounds

case class Parallelograms(elements: Set[BaseParallelogram]) extends ImgArea {
  override def contains(p: com.github.skac112.vgutils.Point): Boolean =
    elements.exists(_.contains(p))

  override lazy val bounds = Some(elements.foldLeft(Bounds.empty) {(curr_bounds, part) => curr_bounds + part.bounds.getOrElse(Bounds.inf)})
}
