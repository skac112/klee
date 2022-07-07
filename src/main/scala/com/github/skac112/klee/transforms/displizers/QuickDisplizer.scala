package com.github.skac112.klee.transforms.displizers

import cats.Monad
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.{Color, Point}

case class QuickDisplizer[I <: O, O, M[_]: Monad](override val displacement: VectorMap[M],
                                          override val colorDispFun: (I, Point) => M[O],
                                          override val area: ImgArea = WholeArea()) extends Displizer[I, O, M]
