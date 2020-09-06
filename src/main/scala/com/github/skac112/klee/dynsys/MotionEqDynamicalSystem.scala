package com.github.skac112.klee.dynsys

import com.github.skac112.klee.PointMap
import com.github.skac112.vgutils.Point

case class MotionEqDynamicalSystem(override val motionEq: PointMap, override val h: Double) extends GenericMotionEqDynamicalSystem {
}
