package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee.Img
import com.github.skac112.klee.dynsys._
import com.github.skac112.klee.transforms.displacers.Displacer._
import com.github.skac112.vgutils.{Color, Point}

case class DynSysDisplacer(dynSys: DynamicalSystem, time: Double) extends Displacer {
  lazy val timeMap = dynSys.timeMap(-time)
  override def displacement(p: Point) = timeMap(p)
}