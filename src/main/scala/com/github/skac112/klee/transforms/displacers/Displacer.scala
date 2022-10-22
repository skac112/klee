package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee._
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.area.imgpt.QuickPtArea
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.{Color, Point}
import cats.Monad
import cats.implicits._
import cats._

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
abstract class Displacer[I, M[_]: Monad] extends LocalImgTrans[I, M] {
  def displacement: VectorMap[M]
//  override implicit val ev: I <:< I = implicitly(ev: I <:< I)
//  override implicit val evSeq: Seq[I] <:< Seq[I] = implicitly(evSeq: Seq[I] <:< Seq[I])

  /**
    * Default area is the whole area.
    * @return
    */
  def area: ImgArea = WholeArea()

  protected def dispVectFor(imgPt: ImgPoint[I, M]) = if (imgPt.land) displacement else invDisplacement

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]): ImgPoint[I, M] = {
    val dispM: M[Point] = for {
      pt <- ip.point
      disp <- dispVectFor(ip)(pt)
    } yield disp

    val dpM = ptSumM(ip.point, dispM)

    if (ip.land) {
      InstantImgPoint(ip.point, imgForPtM(img, dpM), true)
    } else {
      InstantImgPoint(dpM, ip.color, false)
    }
  }

  private def ptSumM(ptM1: M[Point], ptM2: M[Point]) = for {
    pt1 <- ptM1
    pt2 <- ptM2
  } yield pt1 + pt2

  private def imgForPtM(img: Img[I, M], ptM: M[Point]): M[I] = for {
    pt <- ptM
    value <- img(pt)
  } yield value

  def applyToAirInArea(droplet: PureImgPoint[I]): M[PureImgPoint[I]] = for {
    invDisp <- invDisplacement(droplet.point)    
  } yield InstantPureImgPoint[I](droplet.point + invDisp, droplet.color)

  def invDisplacement: VectorMap[M] = ???

  override def applyToAir(img: Img[I, M]): M[PureImgPoints[I]] = for {
    air_pts <- img.air
    img_pts <- applyToImgPtArea(img, QuickPtArea[I, M](air_pts map { pip: PureImgPoint[I] =>
      InstantImgPoint(m.pure(pip.point), m.pure(pip.color), false) }, WholeArea()))
  } yield img_pts

  override def applyBatchInArea(img: Img[I, M], imgPoints: ImgPoints[I, M]): M[PureImgPoints[I]] = {
    val pt_area = QuickPtArea[Point, M](imgPoints map { ip: ImgPoint[I, M] =>
      InstantImgPoint(ip.point, ip.point, ip.land) }, area)

    for {
      disps <- displacement.applyBatchArea(pt_area)
      disp_pts = for {
        disp <- disps
      } yield disp.point + disp.color
      img_colors <- img.applyBatch(disp_pts)
      new_m_ips = (imgPoints zip disp_pts zip img_colors) map { case ((ip, disp_p), img_col) => for {
        ip_pt <- ip.point
        ip_col <- ip.color
      } yield InstantPureImgPoint(if (ip.land) ip_pt else disp_p, if (ip.land) img_col else ip_col, ip.land) }
      new_ips <- new_m_ips.toVector.sequence
    } yield new_ips
  }
}
