package com.github.skac112.klee.transcomb

import cats.Monad
import cats.implicits.*
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LocalImgTrans}
import com.github.skac112.klee.area.img.{MultiPartArea, WholeArea}
import com.github.skac112.vgutils.{ColorVector, Point}
import cats.Id
import cats.implicits.*

import scala.language.postfixOps

case class MultiLocalMixer[M[_]](
                                             localTransforms: Seq[LocalImgTrans[M]],
                                             mixingFun: (ColorVector, ColorVector) => M[ColorVector]) extends LocalImgTrans[M] {
  override def area = MultiPartArea(localTransforms map (_.area) toSet)
  
//override def area = WholeArea()

  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = {
    val color_m = for {
      pt <- ip.point
      color <- colorMFor(img, localTransforms, pt)
    } yield color
    InstantImgPoint(ip.point, color_m)
  }

  private def colorMFor(img: Img[M], transforms: Seq[LocalImgTrans[M]], pt: Point)(implicit m: Monad[M]): M[ColorVector] =
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
