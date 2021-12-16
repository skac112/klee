package com.github.skac112.klee

import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.pt.PtArea
import com.github.skac112.vgutils.{Color, Point}

/**
  * ImgTrans which changes only a part of an image leaving the rest unmodified.
  */
trait LocalImgTrans[T] extends ImgTrans[T] {
  def area: ImgArea

  def apply(img: Img[T]) = new Img[T] {
    override def apply(p: Point) = if (area contains p) {
      applyInArea(img, p) }
    else {
      img(p)
    }

    override def applyBatchArea(ptArea: PtArea): Seq[T] = {
      val (in, out, unknown, merge_fun) = ptArea.partByIntersect[T](area)
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

  def applyInArea(img: Img[T], p: Point): T
  def applyBatchInArea(img: Img[T], points: Points): Seq[T] = points map { applyInArea(img, _) }
}
