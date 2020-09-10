package com.github.skac112.klee.dynsys

import com.github.skac112.klee.dynsys.vectormaps.VectorMap
import com.github.skac112.vgutils.Point

case class MotionEqDynamicalSystem(override val motionEq: VectorMap, override val h: Double) extends GenericMotionEqDynamicalSystem {
}
