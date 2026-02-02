package com.github.skac112.klee.transforms.sprite

import cats.Monad
import com.github.skac112.klee.{Img, ImgPoint}
import com.github.skac112.klee.area.img.BaseParallelogram

case class QuickSimpleSprite[M[_]](
                              spriteArea: BaseParallelogram,
                              applyInAreaFun: (img: Img[M], ip: ImgPoint[M]) => ImgPoint[M]) extends SimpleSprite[M] {
  override def applyInArea(img: Img[M], ip: ImgPoint[M])(using m: Monad[M]): ImgPoint[M] = applyInAreaFun(img, ip)
}
