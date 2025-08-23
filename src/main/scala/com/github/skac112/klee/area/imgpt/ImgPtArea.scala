package com.github.skac112.klee.area.imgpt

import com.github.skac112.klee._
import com.github.skac112.klee.ImgPoint
import cats._
import cats.implicits._
import cats.Monad
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.vgutils.Bounds

object ImgPointArea {
    type JoinFun[M[_]] = (PureImgPoints, PureImgPoints, PureImgPoints) => PureImgPoints
    type PartFunRes[M[_]] = M[(ImgPtArea[M], ImgPtArea[M], ImgPtArea[M], JoinFun[M])]
}

abstract class ImgPtArea[M[_]: Monad] {
  lazy val m = summon[Monad[M]]
  import ImgPointArea._
//  type ThisPartFunRes[O] = PartFunRes[O, M]
  type ThisPartFunRes= M[(ImgPtArea[M], ImgPtArea[M], ImgPtArea[M], JoinFun[M])]
  type ThisPureRes = (ImgPtArea[M], ImgPtArea[M], ImgPtArea[M], JoinFun[M])
  def imgPoints: ImgPoints[M]
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
//  def cut(imgArea: ImgArea): ImgPtArea[M] = new ImgPtArea[M] {
//    override val area = imgArea
//    override val imgPoints = ImgPtArea.this.imgPoints filter { (ip: ImgPoint[M]) => imgArea.contains(ip.point) }
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
  def partByIntersect(imgArea: ImgArea): ThisPartFunRes =
    println("partByIntersect")
    area.containedIn(imgArea) match {
      // area of points inside a given area
      case Some(true) => println("partInside"); partInside
      case _ => area.outsideOf(imgArea) match {
        // area of points outside a given area
        case Some(true) => println("part outside"); partOutside
        // it can't be assured that area of points fully inside imgArea or fully outside imgArea
        case _ => {
          (area.bounds, imgArea.bounds) match {
            case (Some(bounds), Some(img_bounds)) => {
              if (bounds.isOutsideOf(img_bounds)) {
                println("partOutside 2"); partOutside
              }
              else {
                println("partition one by one"); partitionOneByOne(imgArea)
              }
            }
            case _ => {
              println("partition one by one 2"); partitionOneByOne(imgArea)
            }
          }
        }
      }
    }

  private def partitionOneByOne(imgArea: ImgArea): ThisPartFunRes = for {
    points <- pointsM
    src_mask = points map { imgArea contains _ }
    (in_pts, out_pts) = {
      val in_out = (points zip imgPoints) partition { imgArea contains _._1 }
      (in_out._1 map { _._2 }, in_out._2 map { _._2 })
    }

    fun = (inside: PureImgPoints, outside: PureImgPoints, unknown: PureImgPoints) => {
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

    in_area <- BoundsArea.forImgPts[M](in_pts)
    out_area <- BoundsArea.forImgPts[M](out_pts)
  } yield (in_area, out_area, EmptyArea[M](), fun)

  def partInside: ThisPartFunRes = m.pure[ThisPureRes]((this, EmptyArea[M](), EmptyArea[M](),
    (inside: PureImgPoints, outside: PureImgPoints, unknown: PureImgPoints) => inside))

  def partOutside: ThisPartFunRes = m.pure[ThisPureRes]((EmptyArea[M](), this, EmptyArea[M](),
    (inside: PureImgPoints, outside: PureImgPoints, unknown: PureImgPoints) => outside))
}
