package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.Img
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows._
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.flows.vectormaps.VectorMap._
import com.github.skac112.klee.transforms.displacers.Displacer._
import com.github.skac112.vgutils.{Color, Point}

case class FlowDisplacer[M[_]](flow: Flow[M], time: Double, argArea: ImgArea = WholeArea()) extends Displacer[M] {
  override def area(implicit m: Monad[M]) = argArea

//  override val m = implicitly[Monad[M]]
  def timeMap(implicit m: Monad[M]) = flow.timeMap(-time)
  override def displacement(implicit m: Monad[M]) = timeMap - VectorMap.identity[M]
}