package com.github.skac112.klee.transforms.sprite

import cats.Monad
import com.github.skac112.klee.{Img, ImgPoint}

case class QuickUnitSprite[M[_]](
  applyInAreaFun: (img: Img[M], ip: ImgPoint[M]) => ImgPoint[M]
) extends UnitSprite[M] {
  override def applyInArea(img: Img[M], ip: ImgPoint[M])(using m: Monad[M]): ImgPoint[M] = applyInAreaFun(img, ip)
}

