package com.github.skac112.klee.examples

import com.github.skac112.klee.drawToFile
import com.github.skac112.klee.dynsys.MotionEqDynamicalSystem
import com.github.skac112.klee.dynsys.vectormaps.{LocalLinear, VectorMap}
import com.github.skac112.klee.images.{Fill, Lines}
import com.github.skac112.klee.transforms.displacers.DynSysDisplacer
import com.github.skac112.vgutils._
import com.github.skac112.vgutils.transform.linear._

import scala.math._

class Example2 {
//  val motion_eq = VectorMap.from(Rotation(.25 * Pi) * UniScale(-.5))
  val lin = Rotation(.3 * Pi) * UniScale(-.8)
//  val lin = Rotation(.25 * Pi)
  val motion_eq = LocalLinear(lin, Point(100, 100), .05)
//  val motion_eq = VectorMap.from(UniScale(-.5))
  val dyn_sys = MotionEqDynamicalSystem(motion_eq, .1)
  val fun = DynSysDisplacer(dyn_sys, 3)
  val src_img = Lines(0, 0, 20, 20, 2, Color.black, Color.white)
  val dst_img = fun(src_img)
  val n = 500
  drawToFile(dst_img, "example2_1.png", -n/2, n/2, -n/2, n/2, n, n)
}
