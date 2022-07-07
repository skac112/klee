package com.github.skac112.klee.flows

import cats.Monad
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.Point

case class MotionEqFlow[M[_]: Monad](override val motionEq: VectorMap[M], override val h: Double) extends GenericMotionEqFlow {
}
