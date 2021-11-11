package com.github.skac112.klee

import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.pt.PtArea
import com.github.skac112.vgutils.{Color, Point}

/**
  * ImgTrans which changes only a part of an image leaving the rest unmodified.
  */
trait LocalImgTrans extends ImgTrans {
  def area: ImgArea

  def apply(img: Img) = new Img {
    override def apply(p: Point) = if (area contains p) {
      applyInArea(img, p) }
    else {
      img(p)
    }

    override def applyBatchArea(ptArea: PtArea): Seq[Color] = {
      val (in, out, unknown, merge_fun) = ptArea.partByIntersect[Color](area)
      // points transformed by this trans - inside trans area
      val in_colors = applyBatchInArea(img, in.points)
      // point taken from input image - outside trans area
      val out_colors = img.applyBatchArea(out)
      // colors for unknown area (colors themselves are 'known')
      val unknown_colors = unknown.points map { p =>
        if (area contains p) {
          applyInArea(img, p)
        }
        else {
          img(p)
        }}
      merge_fun(in_colors, out_colors, unknown_colors)
    }
  }

  def applyInArea(img: Img, p: Point): Color
  def applyBatchInArea(img: Img, points: Points): Colors = points map { applyInArea(img, _) }
}
