package com.github.skac112.klee.transforms.sprite

import com.github.skac112.klee.LocalImgTrans
import com.github.skac112.klee.area.img.{BaseParallelogram, ImgArea}

trait SimpleSprite[M[_]] extends Sprite[M] {
  def spriteArea: BaseParallelogram
  override def area: ImgArea = spriteArea
}
