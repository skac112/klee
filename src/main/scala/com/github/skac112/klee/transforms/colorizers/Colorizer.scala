package com.github.skac112.klee.transforms.colorizers.gradients
import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.img.WholeArea
import com.github.skac112.klee.Img
import com.github.skac112.vgutils.Point
import com.github.skac112.klee.LocalImgTrans

abstract class Colorizer[I <: O, O,  M[_]: Monad] extends LocalImgTrans[I, O, M] {
  def area: ImgArea = WholeArea()
  def colorFun: (I) => M[O]
  override def applyInArea(img: Img[I, M], p: Point): M[O] = img(p).flatMap(colorFun)
}
