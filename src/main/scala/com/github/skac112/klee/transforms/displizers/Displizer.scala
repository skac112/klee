package com.github.skac112.klee.transforms.displizers

import cats.Monad
import cats.implicits.*
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.{Img, LocalImgTrans}
import com.github.skac112.vgutils.{Color, ColorVector, Point}

abstract class Displizer[M[_]] extends LocalImgTrans[M] {
//  override val m =
//  override val m = implicitly[Monad[M]]
  override def area: ImgArea = WholeArea()
  def displacement(implicit m: Monad[M]): VectorMap[M]
  def colorDispFun: (ColorVector, Point) => M[ColorVector]

  override def applyInArea(img: Img[M], p: Point)(using m: Monad[M]) = for {
    disp <- displacement.apply(p)
    disp_color <- img(p + disp)
    out <- colorDispFun(disp_color, disp)
  } yield out
}
