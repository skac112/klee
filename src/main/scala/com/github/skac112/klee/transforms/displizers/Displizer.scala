package com.github.skac112.klee.transforms.displizers

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.{Img, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

abstract class Displizer[I <: O, O, M[_]: Monad] extends LocalImgTrans[I, O, M] {
//  override val m =
//  override val m = implicitly[Monad[M]]
  override def area: ImgArea = WholeArea()
  def displacement: VectorMap[M]
  def colorDispFun: (I, Point) => M[O]

  override def applyInArea(img: Img[I, M], p: Point) = for {
    disp <- displacement(p)
    disp_color <- img(p + disp)
    out <- colorDispFun(disp_color, disp)
  } yield out
}
