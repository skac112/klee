package com.github.skac112.klee.area.imgpt

import cats.Monad
import cats.implicits._
import com.github.skac112.klee._
import com.github.skac112.vgutils.Bounds

object BoundsArea {
  def forImgPts[I, M[_]: Monad](imgPoints: ImgPoints[I, M]) = for {
    pts <- unwrapPoints(imgPoints)
  } yield BoundsArea(Bounds.forPts(pts.toSet), imgPoints)
}

/**
  * Points area of points contained in given Bounds. Unlike AxisGrid, points need not to form rectangular mesh.
  * @param givenBounds
  * @param imgPoints
  */
case class BoundsArea[I, M[_]: Monad](givenBounds: Bounds, override val imgPoints: ImgPoints[I, M]) extends ImgPtArea[I, M] {
  override def area = com.github.skac112.klee.area.img.BoundsArea(givenBounds)
}
