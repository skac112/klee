package com.github.skac112.klee.transforms.displizers

import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.{Color, Point}

case class QuickDisplazer[T](override val displacement: VectorMap,
                          override val colorDispFun: (T, Point) => T,
                          override val area: ImgArea = WholeArea()) extends Displizer[T]
