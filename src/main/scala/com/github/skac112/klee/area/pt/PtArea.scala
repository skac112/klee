package com.github.skac112.klee.area.pt

import com.github.skac112.klee.Points
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.vgutils.{Bounds, Point}

trait PtArea {
  def points: Points

  def area: ImgArea

  def intersection(other: ImgArea) = area.intersection(other) map cut

  lazy val bounds = area.bounds match {
    case Some(b) => b
    case _ => Bounds.forPts(points.toSet)
  }

  /**
    * Creates new point area with given image area containing only points from this area which
    * lie inside given image area
    *
    * @param imgArea
    * @return
    */
  def cut(imgArea: ImgArea): PtArea = new PtArea {
    override val area = imgArea
    override val points = PtArea.this.points filter {
      imgArea.contains
    }
  }

  /**
    * Partitions this point area using given image area into three separate point areas:
    * - one which surely lies within given area
    * - one which surely lies outside given area
    * - one which can lie inside or outside given area either partially or as a whole
    * Each point of this area should belong to exactly one of these three areas.
    * Fourth element of tuple is a merging function - it's application to three processed sequences of elements
    * returns sequence of all elements with proper order matching order of corresponding points in imgArea
    */
  def partByIntersect[T](imgArea: ImgArea): (PtArea, PtArea, PtArea, (scala.collection.Seq[T], scala.collection.Seq[T], scala.collection.Seq[T]) => scala.collection.Seq[T]) =
    area.containedIn(imgArea) match {
      // area of points inside a given area
      case Some(true) => partInside[T]
      case _ => area.outsideOf(imgArea) match {
        // area of points outside a given area
        case Some(true) => partOutside[T]
        // it can't be assured that area of points fully inside imgArea or fully outside imgArea
        case _ => {
          (area.bounds, imgArea.bounds) match {
            case (Some(bounds), Some(img_bounds)) => {
              if (bounds.isOutsideOf(img_bounds)) {
                partOutside[T]
              }
              else {
                partitionOneByOne[T](imgArea)
              }
            }
            case _ => partitionOneByOne[T](imgArea)
          }
        }
      }
    }

  private def partitionOneByOne[T](imgArea: ImgArea) = {
    // this method partitions points fully, i.e. no 'unknown' area is created, but it
    // degrades source image area (returning just two rectangular bounds) and is also
    // rather costly
    val src_mask = points map { imgArea contains _ }
    val ((in_pts, out_pts)) = points partition { imgArea contains _ }

    val fun = (inside: scala.collection.Seq[T], outside: scala.collection.Seq[T], unknown: scala.collection.Seq[T]) => {
        var inside_idx = 0
        var outside_idx = 0

        src_mask map { if (_) {
                inside_idx = inside_idx + 1
                inside(inside_idx - 1)
            } else {
                outside_idx = outside_idx + 1
                outside(outside_idx - 1)
            }
        }
    }
    (BoundsArea.forPts(in_pts), BoundsArea.forPts(out_pts), EmptyArea(), fun)
  }

  def partInside[T]: (PtArea, PtArea, PtArea, (scala.collection.Seq[T], scala.collection.Seq[T], scala.collection.Seq[T]) => scala.collection.Seq[T]) =
    (this, EmptyArea(), EmptyArea(), (inside: scala.collection.Seq[T], outside: scala.collection.Seq[T], unknown: scala.collection.Seq[T]) => inside)

  def partOutside[T]: (PtArea, PtArea, PtArea, (scala.collection.Seq[T], scala.collection.Seq[T], scala.collection.Seq[T]) => scala.collection.Seq[T]) =
    (EmptyArea(), this, EmptyArea(), (inside: scala.collection.Seq[T], outside: scala.collection.Seq[T], unknown: scala.collection.Seq[T]) => outside)
}
