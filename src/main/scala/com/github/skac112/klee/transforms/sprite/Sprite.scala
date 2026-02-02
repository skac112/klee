package com.github.skac112.klee.transforms.sprite

import com.github.skac112.klee.LocalImgTrans
import com.github.skac112.klee.area.img.{BaseParallelogram, ImgArea, Parallelograms, SpriteArea}

trait Sprite[M[_]] extends LocalImgTrans[M] {
  def spriteArea: SpriteArea
//  def areaElements: Parallelograms
  override def area: ImgArea = spriteArea
}
