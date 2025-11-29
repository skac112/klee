package com.github.skac112.klee.transcomb.sprite

import com.github.skac112.klee.area.img.{BaseParallelogram, ImgArea, Parallelograms}
import com.github.skac112.klee.{ImgTrans, LocalImgTrans}
import com.github.skac112.klee.transcomb.sprite.CompositeSprite.Sprite
import com.github.skac112.klee.transforms.SimpleSprite

import scala.language.postfixOps

object CompositeSprite {
  type Sprite[M[_]] = CompositeSprite[M] | SimpleSprite[M]
  type SpriteArea = BaseParallelogram | Set[BaseParallelogram]
}

trait CompositeSprite[M[_]](elements: scala.collection.Seq[Sprite[M]]) extends LocalImgTrans[M] {
  private def areaElements: Set[BaseParallelogram] = elements.toSet flatMap {
    case cs: CompositeSprite[M] => cs.areaElements
    case ss: SimpleSprite[M] => Set(ss.spriteArea)
  }

  override def area: ImgArea = Parallelograms(areaElements)
}
