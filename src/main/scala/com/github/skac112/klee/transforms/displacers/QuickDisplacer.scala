package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.transforms.displizers.Displizer
import com.github.skac112.vgutils.{Color, Point}

case class QuickDisplacer[M[_]: Monad](
                                           argDisplacement: VectorMap[M],
                                           argArea: ImgArea = WholeArea()) extends Displacer[M] {
  override def area(implicit m: Monad[M]) = argArea

  override def displacement(implicit m: Monad[M]) = argDisplacement
//  override val m = implicitly(Monad[M])

}

