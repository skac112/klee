package com.github.skac112.klee.transcomb

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.klee.area.img.{MultiPartArea, WholeArea}
import com.github.skac112.vgutils.Point

import scala.language.postfixOps

case class MultiLocalMixer[I, M[_]](
                                             localTransforms: Seq[LocalImgTrans[I, M]],
                                             mixingFun: (I, I) => M[I]) extends LocalImgTrans[I, M] {
  override def area(implicit m: Monad[M]) = MultiPartArea(localTransforms map (_.area) toSet)
//override def area = WholeArea()

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M])(implicit m: Monad[M]): ImgPoint[I, M] = {
    val color_m = for {
      pt <- ip.point
      color <- colorMFor(img, localTransforms, pt)
    } yield color
    InstantImgPoint(ip.point, color_m)
  }

  private def colorMFor(img: Img[I, M], transforms: Seq[LocalImgTrans[I, M]], pt: Point)(implicit m: Monad[M]): M[I] =
    transforms.foldLeft(img(pt))((color_m, transform) => if (transform.area.contains(pt)) {
      // point in area of current local transform - mixing
      for {
        color1 <- color_m
        color2 <- transform.applyInArea(img, pt)
        mixed_color_m <- mixingFun(color1, color2)
      } yield mixed_color_m
    } else {
      // point outside area of current local transform - color stays the same
      color_m
    })
}
