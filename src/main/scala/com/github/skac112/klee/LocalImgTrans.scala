package com.github.skac112.klee

import com.github.skac112.vgutils.{Color, Point}

/**
  * ImgTrans which changes only a part of an image leaving the rest unmodified.
  */
trait LocalImgTrans extends ImgTrans {
  def area: ImgArea

  def apply(img: Img) = new Img {
    override def apply(p: Point) = if (area.contains(p)) {
      applyInArea(img, p: Point)
    } else {
      img(p)
    }

    override def applyBatch(points: Points) = {
      // tymczasowo!!!
      points map apply _
//      if (points.area containedIn area) {
//        // all points if area of trans
//        applyBatchInArea(img, points)
//      }
//      else if (points.area outsideOf area) {
//        // all points outside area of trans
//        img.applyBatch(points)
//      }
//      else {
//        // points area intersects with trans area - each point is evaluated separately
//
//      }
    }
  }

  def applyInArea(img: Img, p: Point): Color
  def applyBatchInArea(img: Img, points: Points): Colors = points map {applyInArea(img, _)}
}
