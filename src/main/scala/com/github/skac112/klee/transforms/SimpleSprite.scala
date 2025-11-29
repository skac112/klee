package com.github.skac112.klee.transforms

import com.github.skac112.klee.LocalImgTrans
import com.github.skac112.klee.area.img.{BaseParallelogram, ImgArea}

trait SimpleSprite[M[_]] extends LocalImgTrans[M] {
  def spriteArea: BaseParallelogram
  override def area: ImgArea = spriteArea
}
