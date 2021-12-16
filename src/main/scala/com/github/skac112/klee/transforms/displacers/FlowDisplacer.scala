package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee.Img
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows._
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.transforms.displacers.Displacer._
import com.github.skac112.vgutils.{Color, Point}

case class FlowDisplacer(flow: Flow, time: Double, override val area: ImgArea = WholeArea()) extends Displacer {
  lazy val timeMap = flow.timeMap(-time)
  override def displacement = timeMap - VectorMap.identity
}