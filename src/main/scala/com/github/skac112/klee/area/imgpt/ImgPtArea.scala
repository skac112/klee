package com.github.skac112.klee.area.imgpt

import com.github.skac112.klee._
import com.github.skac112.klee.ImgPoint
import cats._
import cats.implicits._
import cats.Monad
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.vgutils.Bounds

object ImgPointArea {
    type JoinFun[O, M[_]] = (PureImgPoints[O], PureImgPoints[O], PureImgPoints[O]) => PureImgPoints[O]
    type PartFunRes[I, O, M[_]] = M[(ImgPtArea[I, M], ImgPtArea[I, M], ImgPtArea[I, M], JoinFun[O, M])]
}

abstract class ImgPtArea[I, M[_]: Monad] {
  lazy val m = implicitly[Monad[M]]
  import ImgPointArea._
//  type ThisPartFunRes[O] = PartFunRes[I, O, M]
  type ThisPartFunRes[O] = M[(ImgPtArea[I, M], ImgPtArea[I, M], ImgPtArea[I, M], JoinFun[O, M])]
  type ThisPureRes[O] = (ImgPtArea[I, M], ImgPtArea[I, M], ImgPtArea[I, M], JoinFun[O, M])
  def imgPoints: ImgPoints[I, M]
  def area: ImgArea
  lazy val pointsM = (imgPoints map { _.point }).toVector.sequence

//   def intersection[I2, M2[_]: Monad](other: ImgPtArea[I2, M2]) = area.intersection(other.area) map cut

  lazy val boundsM: M[Bounds] = area.bounds match {
    case Some(b) => m.pure(b)
    case _ => for {
      pts <- pointsM
    } yield Bounds.forPts(pts.toSet)
  }

  /**
    * Creates new point area with given image area containing only points from this area which
    * lie inside given image area
    *
    * @param imgArea
    * @return
    */
//  def cut(imgArea: ImgArea): ImgPtArea[I, M] = new ImgPtArea[I, M] {
//    override val area = imgArea
//    override val imgPoints = ImgPtArea.this.imgPoints filter { (ip: ImgPoint[I, M]) => imgArea.contains(ip.point) }
//  }

  /**
    * Partitions this point area using given image area into three separate point areas:
    * - one which surely lies within given area
    * - one which surely lies outside given area
    * - one which can lie inside or outside given area either partially or as a whole
    * Each point of this area should belong to exactly one of these three areas.
    * Fourth element of tuple is a merging function - it's application to three processed sequences of elements
    * returns sequence of all elements with proper order matching order of corresponding points in imgArea
    */
  def partByIntersect[O](imgArea: ImgArea): ThisPartFunRes[O] =
    area.containedIn(imgArea) match {
      // area of points inside a given area
      case Some(true) => partInside
      case _ => area.outsideOf(imgArea) match {
        // area of points outside a given area
        case Some(true) => partOutside
        // it can't be assured that area of points fully inside imgArea or fully outside imgArea
        case _ => {
          (area.bounds, imgArea.bounds) match {
            case (Some(bounds), Some(img_bounds)) => {
              if (bounds.isOutsideOf(img_bounds)) {
                partOutside
              }
              else {
                partitionOneByOne(imgArea)
              }
            }
            case _ => partitionOneByOne(imgArea)
          }
        }
      }
    }

  private def partitionOneByOne[O](imgArea: ImgArea): ThisPartFunRes[O] = for {
    points <- pointsM
    src_mask = points map { imgArea contains _ }
    (in_pts, out_pts) = {
      val in_out = (points zip imgPoints) partition { imgArea contains _._1 }
      (in_out._1 map { _._2 }, in_out._2 map { _._2 })
    }

    fun = (inside: PureImgPoints[O], outside: PureImgPoints[O], unknown: PureImgPoints[O]) => {
      var inside_idx = 0
      var outside_idx = 0

      src_mask map { if (_) {
        inside_idx = inside_idx + 1
        inside(inside_idx - 1)
      } else {
        outside_idx = outside_idx + 1
        outside(outside_idx - 1)
      }}
    }

    in_area <- BoundsArea.forImgPts[I, M](in_pts)
    out_area <- BoundsArea.forImgPts[I, M](out_pts)
  } yield (in_area, out_area, EmptyArea[I, M](), fun)

  def partInside[O]: ThisPartFunRes[O] = m.pure[ThisPureRes[O]]((this, EmptyArea[I, M](), EmptyArea[I, M](),
    (inside: PureImgPoints[O], outside: PureImgPoints[O], unknown: PureImgPoints[O]) => inside))

  def partOutside[O]: ThisPartFunRes[O] = m.pure[ThisPureRes[O]]((EmptyArea[I, M](), this, EmptyArea[I, M](),
    (inside: PureImgPoints[O], outside: PureImgPoints[O], unknown: PureImgPoints[O]) => outside))
}
