package com.github.skac112.klee.transforms.displizers

import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.dynsys.vectormaps.VectorMap
import com.github.skac112.klee.{Img, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

trait Displizer extends LocalImgTrans {
  override def area: ImgArea = WholeArea()

  def displacement: VectorMap

  def colorDispFun: (Color, Point) => Color

  override def applyInArea(img: Img, p: Point) = {
    val disp = displacement(p)
    val disp_color = img(p + disp)
    colorDispFun(disp_color, disp)
  }
}
