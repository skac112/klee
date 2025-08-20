package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee.*
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.area.imgpt.QuickPtArea
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.{Color, ColorVector, Point}
import cats.Monad
import cats.implicits.*
import cats.*

import scala.collection.parallel.CollectionConverters.IterableIsParallelizable
import scala.collection.parallel.immutable.ParVector

object Displacer {
  type DispColorChangeFun[T, M[_]] = (Point, Point, Img[T, M]) => T
}

/**
  * Abstract base class for displacers. Value of image function for a point is taken from another point.
  * This other point is calculated as a displacement from given point. So, displacement value (vector) for a given point
  * determines value of a function (color) for given point and image.
  * CAUTION: Displacement vector is not equal to vector which transforms location of every point to a new
  * location along with its color. Value of displacement is the opposite of transformation of location of points. In such a model 
  * (which can be for example understood as an application of dynamical
  * system to image for some time shift) the displacement vector is an opposite of this first vector taken
  * for given end point. For example: if a displacer transform for given point p1 translates it to point p2 = p1 +
  * transl, the displacement vector for point p2 (not p1) is equal to -transl. So, the displacement is a "lookup" vector
  * which is used to take a value from (combined with location of base point). 
  */
abstract class Displacer[M[_]] extends LocalImgTrans[M] {
  def displacement(implicit m: Monad[M]): VectorMap[M]
//  override implicit val ev: I <:< I = implicitly(ev: I <:< I)
//  override implicit val evSeq: Seq <:< Seq = implicitly(evSeq: Seq <:< Seq)

  /**
    * Default area is the whole area.
    * @return
    */
  override def area(implicit m: Monad[M]): ImgArea = WholeArea()

  protected def dispVectFor(imgPt: ImgPoint[M])(implicit m: Monad[M]) = if (imgPt.land) displacement else invDisplacement

  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = {
    val dispM: M[Point] = for {
      pt <- ip.point
      disp <- dispVectFor(ip).apply(pt)
    } yield disp

    val dpM = ptSumM(ip.point, dispM)

    if (ip.land) {
      InstantImgPoint(ip.point, imgForPtM(img, dpM))
    } else {
      InstantImgPoint(dpM, ip.color, false)
    }
  }

  private def ptSumM(ptM1: M[Point], ptM2: M[Point])(implicit m: Monad[M]) = for {
    pt1 <- ptM1
    pt2 <- ptM2
  } yield pt1 + pt2

  private def imgForPtM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]): M[ColorVector] = for {
    pt <- ptM
    value <- img(pt)
  } yield value

//   def applyToAirInArea(droplet: PureImgPoint): M[PureImgPoint] = for {
//     invDisp <- invDisplacement(droplet.point)    
//   } yield InstantPureImgPoint(droplet.point + invDisp, droplet.color)

  def invDisplacement: VectorMap[M] = ???

  override def applyToAir(img: Img[M])(implicit m: Monad[M]): M[PureImgPoints] = for {
    air_pts <- img.air
    img_pts <- applyToImgPtArea(img, QuickPtArea[M](air_pts map { (pip: PureImgPoint) =>
      InstantImgPoint(m.pure(pip.point), m.pure(pip.color), false) }, WholeArea()))
  } yield img_pts

  override def applyBatchInArea(img: Img[M], imgPoints: ImgPoints[M])(implicit m: Monad[M]): M[PureImgPoints] = {
//    val pt_area = QuickPtArea[Point, M](imgPoints map { (ip: ImgPoint[M]) =>
//      InstantImgPoint(ip.point, ip.point, ip.land) }, area)

    // sequence of boolean's signalling that appropriate imgPoint is land-point or air-point.
    val land_air_mask = imgPoints map { _.land }
    var land_idx = 0
    var air_idx = 0
    val disps = displacement.applyBatchArea(imgPoints)
    
    val new_m_ips = land_air_mask.zipWithIndex map { case (is_land, idx) =>
//      new_m_ips = ((land_air_mask zip (0 until land_air_mask.length)).par) map { case (is_land, idx) =>
        if (is_land) {
          for {
            // for land points new point is the same as old
            pt <- imgPoints(idx).point
            disp <- disps(idx)
            // for land point new color is taken from displaced land point
            color <- img(pt + disp)
          } yield InstantPureImgPoint(pt, color, true)
        } else {
          for {
            // for air points new point is displaced by vector from inverse displacement
            // and new color is the same as old
            pt <- imgPoints(idx).point
            disp <- disps(idx)
            color <- imgPoints(idx).color
          } yield InstantPureImgPoint(pt + disp, color, false)
        }
      }

    for {

//      new_m_ips = (imgPoints zip disp_pts zip img_colors) map { case ((ip, disp_p), img_col) => for {
//        ip_pt <- ip.point
//        ip_col <- ip.color
//      } yield InstantPureImgPoint(if (ip.land) ip_pt else disp_p, if (ip.land) img_col else ip_col, ip.land) }
      new_ips <- new_m_ips.toVector.sequence
    } yield new_ips
  }
}
