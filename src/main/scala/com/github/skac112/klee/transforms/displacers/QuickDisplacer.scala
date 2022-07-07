package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.transforms.displizers.Displizer
import com.github.skac112.vgutils.{Color, Point}

case class QuickDisplacer[T, M[_]: Monad](override val displacement: VectorMap[M],
                          override val area: ImgArea = WholeArea()) extends Displacer[T, M] {
//  override val m = implicitly(Monad[M])

}

