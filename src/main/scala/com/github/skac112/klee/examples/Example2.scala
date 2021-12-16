package com.github.skac112.klee.examples

import com.github.skac112.klee.{drawToFile, trivialColorFun}
import com.github.skac112.klee.flows.MotionEqFlow
import com.github.skac112.klee.flows.vectormaps.{LocalLinear, VectorMap}
import com.github.skac112.klee.images.{Fill, Lines}
import com.github.skac112.klee.transforms.displacers.{FlowDisplacer, QuickDisplacer}
import com.github.skac112.vgutils._
import com.github.skac112.vgutils.transform.linear._

import scala.math._

class Example2 {
  val lin = Rotation(2*Pi) * UniScale(.9)
//  val lin = Rotation(1.4*Pi)
  val motion_eq = LocalLinear(lin, Point(.5, .5), 2.0) - VectorMap.identity
//  val vect_map = LocalLinear(lin, Point(.5, .5), 3.0)
//  val vect_map = VectorMap.from(lin)
  val dyn_sys = MotionEqFlow(motion_eq, .02)
  val fun = FlowDisplacer[Color](dyn_sys, 4.0)
//  val fun = QuickDisplacer(vect_map - VectorMap.identity)
  val src_img = Lines[Color](0, 0, .05, .05, .005, Color.black, Color.white)
  val dst_img = fun(src_img)
  val n = 300
  drawToFile(dst_img, trivialColorFun, s"example4.png", 0.0, 1.0, 0.0, 1.0, n, n)
}
