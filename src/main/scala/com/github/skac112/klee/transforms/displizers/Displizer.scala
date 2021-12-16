package com.github.skac112.klee.transforms.displizers

import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.{Img, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

trait Displizer[T] extends LocalImgTrans[T] {
  override def area: ImgArea = WholeArea()

  def displacement: VectorMap

  def colorDispFun: (T, Point) => T

  override def applyInArea(img: Img[T], p: Point) = {
    val disp = displacement(p)
    val disp_color = img(p + disp)
    colorDispFun(disp_color, disp)
  }
}
