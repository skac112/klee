package com.github.skac112.klee.transforms.bands

import com.github.skac112.vgutils.Point

object SectionBand {
  type Curve = Double => Point
  case class BandSection[I, M[_]](p1: Point, p2: Point)
}

import SectionBand._

trait SectionBand {
  def trajectory: Curve
//  def
}
