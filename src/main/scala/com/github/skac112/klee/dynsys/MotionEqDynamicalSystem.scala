package com.github.skac112.klee.dynsys

import com.github.skac112.vgutils.Point

case class MotionEqDynamicalSystem(motionEqFun: (Point) => Point, override val h: Double) extends GenericMotionEqDynamicalSystem {
  override def motionEq(p: Point) = motionEqFun(p)
}
