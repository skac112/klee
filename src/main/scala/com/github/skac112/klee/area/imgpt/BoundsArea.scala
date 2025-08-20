package com.github.skac112.klee.area.imgpt

import cats.Monad
import cats.implicits._
import com.github.skac112.klee._
import com.github.skac112.vgutils.Bounds

object BoundsArea {
  def forImgPts[M[_]: Monad](imgPoints: ImgPoints[M]) = for {
    pts <- unwrapPoints(imgPoints)
  } yield BoundsArea(Bounds.forPts(pts.toSet), imgPoints)
}

/**
  * Points area of points contained in given Bounds. Unlike AxisGrid, points need not to form rectangular mesh.
  * @param givenBounds
  * @param imgPoints
  */
case class BoundsArea[M[_]: Monad](givenBounds: Bounds, override val imgPoints: ImgPoints[M]) extends ImgPtArea[M] {
  override def area = com.github.skac112.klee.area.img.BoundsArea(givenBounds)
}
