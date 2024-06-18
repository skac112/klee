package com.github.skac112.klee.transforms.bands

import com.github.skac112.klee.LocalImgTrans
import com.github.skac112.vgutils.Point

object Band {
  type BandFun[I, M[_]] = Point => M[(Point, I)]
}

import Band._

trait Band[I, M[_]] extends LocalImgTrans[I, M] {
  def bandFun: BandFun[I, M]
}
