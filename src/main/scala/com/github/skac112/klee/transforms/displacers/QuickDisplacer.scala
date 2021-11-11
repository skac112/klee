package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.dynsys.vectormaps.VectorMap
import com.github.skac112.klee.transforms.displizers.Displizer
import com.github.skac112.vgutils.{Color, Point}

case class QuickDisplacer(override val displacement: VectorMap,
                          override val area: ImgArea = WholeArea()) extends Displacer

