package com.github.skac112.klee.transcomb.sprite

import cats.Monad
import com.github.skac112.klee.transcomb.sprite.CompositeSprite.Sprite
import com.github.skac112.klee.transforms.SimpleSprite
import com.github.skac112.klee.{Img, ImgPoint, ImgTrans}

case class Composition[M[_]](elements: scala.collection.Seq[Sprite[M]]) extends CompositeSprite[M](elements) {

  lazy val fun: ImgTrans[M] = elements.reduce { (acc, element) => new ImgTrans[M] {
    override def apply(img: Img[M])(using m: Monad[M]): Img[M] = element(acc(img))}}

  override def apply(img: Img[M])(using m: Monad[M]): Img[M] = fun(img)
  def this(element: Sprite[M], times: Int) = this(Seq.fill[Sprite[M]](times)(element))

  override def applyInArea(img: Img[M], ip: ImgPoint[M])(using m: Monad[M]): ImgPoint[M] = apply(img).applyToImgPt(ip)
}

