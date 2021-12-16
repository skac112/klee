package com.github.skac112.klee.flows

import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.Point

case class MotionEqFlow(override val motionEq: VectorMap, override val h: Double) extends GenericMotionEqFlow {
}
