package com.github.skac112.klee.transcomb.sprite

import com.github.skac112.klee.area.img.{BaseParallelogram, ImgArea, Parallelograms, SpriteArea}
import com.github.skac112.klee.{ImgTrans, LocalImgTrans}
import com.github.skac112.klee.transforms.sprite.{SimpleSprite, Sprite}

import scala.language.postfixOps

trait CompositeSprite[M[_]](elements: scala.collection.Seq[Sprite[M]]) extends Sprite[M] {
  private def areaElements: Set[BaseParallelogram] = elements.toSet flatMap {
    case cs: CompositeSprite[M] => cs.areaElements
    case ss: SimpleSprite[M] => Set(ss.spriteArea)
  }

  override def spriteArea: SpriteArea = Parallelograms(areaElements)
}
