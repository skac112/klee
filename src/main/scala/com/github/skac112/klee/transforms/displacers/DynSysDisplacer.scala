package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee.Img
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.dynsys._
import com.github.skac112.klee.dynsys.vectormaps.VectorMap
import com.github.skac112.klee.transforms.displacers.Displacer._
import com.github.skac112.vgutils.{Color, Point}

case class DynSysDisplacer(dynSys: DynamicalSystem, time: Double, override val area: ImgArea = WholeArea()) extends Displacer {
  lazy val timeMap = dynSys.timeMap(-time)
  override def displacement = timeMap - VectorMap.identity
}