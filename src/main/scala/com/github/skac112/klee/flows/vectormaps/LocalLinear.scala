package com.github.skac112.klee.flows.vectormaps

import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.transform.linear.GenericLinear

case class LocalLinear(linear: GenericLinear, center: Point, decay: Double) extends VectorMap {
  override def apply(p: Point) = {
    val p_loc = p - center
    p + (linear(p_loc) * math.exp(-decay*(p_loc.modulus)))
  }
}
