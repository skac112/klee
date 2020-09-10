package com.github.skac112.klee.dynsys.vectormaps

import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.transform.linear.GenericLinear

case class LocalLinear(linear: GenericLinear, center: Point, decay: Double) extends VectorMap {
  lazy val linMap = VectorMap.from(linear)
  override def apply(p: Point) = {
    val p_loc = p - center
    linMap(p_loc) * math.exp(-decay*(p_loc.modulus))
  }
}
