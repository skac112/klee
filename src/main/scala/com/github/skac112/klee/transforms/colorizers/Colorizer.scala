package com.github.skac112.klee.transforms.colorizers.gradients
import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.img.WholeArea
import com.github.skac112.klee.Img
import com.github.skac112.vgutils.Point
import com.github.skac112.klee.LocalImgTrans

/**
  * Colorizer - value of a point (it's "color" - though exact type is not necesarilly a color) depends only on a "color" of a point
  * in input image, so the function in it's application area is wholly determined by the colorFun. 
  */
abstract class Colorizer[I,  M[_]: Monad] extends LocalImgTrans[I, M] {
  def area: ImgArea = WholeArea()
  def colorFun: (I) => M[I]
  override def applyInArea(img: Img[I, M], p: Point): M[I] = img(p).flatMap(colorFun)

//   override def applyToAir(img: Img[I,M]): Seq[Droplet[O,M]] = {
}
