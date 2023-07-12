package com.github.skac112.klee.transforms.displizers

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.{Img, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

abstract class Displizer[I, M[_]] extends LocalImgTrans[I, M] {
//  override val m =
//  override val m = implicitly[Monad[M]]
  override def area(implicit m: Monad[M]): ImgArea = WholeArea()
  def displacement(implicit m: Monad[M]): VectorMap[M]
  def colorDispFun: (I, Point) => M[I]

  override def applyInArea(img: Img[I, M], p: Point)(implicit m: Monad[M]) = for {
    disp <- displacement.apply(p)
    disp_color <- img(p + disp)
    out <- colorDispFun(disp_color, disp)
  } yield out
}
