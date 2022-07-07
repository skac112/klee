package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.Img
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows._
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.flows.vectormaps.VectorMap._
import com.github.skac112.klee.transforms.displacers.Displacer._
import com.github.skac112.vgutils.{Color, Point}

case class FlowDisplacer[T, M[_]: Monad](flow: Flow[M], time: Double, override val area: ImgArea = WholeArea()) extends Displacer[T, M] {
//  override val m = implicitly[Monad[M]]
  lazy val timeMap = flow.timeMap(-time)
  override def displacement = timeMap - VectorMap.identity[M]
}