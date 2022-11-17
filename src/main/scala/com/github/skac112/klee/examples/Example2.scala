package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.{drawToFileOld, trivialColorFun}
import com.github.skac112.klee.flows.MotionEqFlow
import com.github.skac112.klee.flows.vectormaps.{LocalLinear, VectorMap}
import com.github.skac112.klee.images.{Fill, Lines}
import com.github.skac112.klee.transforms.displacers.{FlowDisplacer, QuickDisplacer}
import com.github.skac112.vgutils._
import com.github.skac112.vgutils.transform.linear._

import scala.math._
import cats.implicits._
import cats.Monad._

class Example2 {
  val lin = Rotation(.3*Pi) * UniScale(.9)
  val motion_eq = LocalLinear[Id](lin, Point(.5, .5), 2.0) - VectorMap.identity[Id]
  val dyn_sys = MotionEqFlow[Id](motion_eq, .02)
  val fun = FlowDisplacer[Color, Id](dyn_sys, 1.0)
  val src_img = Lines[Color, Id](0, 0, .05, .05, .005, Color.black, Color.white)
  val dst_img = fun(src_img)
  val n = 300
  drawToFileOld[Color, Id](dst_img, trivialColorFun, s"example2_1.png", 0.0, 1.0, 0.0, 1.0, n, n)
}
