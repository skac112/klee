package com.github.skac112.klee.transforms.displizers

import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.{Color, Point}

case class QuickDisplazer(override val displacement: VectorMap,
                          override val colorDispFun: (Color, Point) => Color,
                          override val area: ImgArea = WholeArea()) extends Displizer
