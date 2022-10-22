package com.github.skac112.klee

import com.github.skac112.klee.area.img.WholeArea
import com.github.skac112.klee.area.imgpt
import com.github.skac112.klee.area.pt.{PtArea, QuickPtArea}
import com.github.skac112.vgutils.Point
import cats.{Applicative, Monad}
import cats.implicits._
import com.github.skac112.klee.area.imgpt.ImgPtArea

trait Img[I, M[_]] extends (Point => M[I]) {
  implicit val m: Monad[M]
  def air: M[PureImgPoints[I]] = m.pure(Seq())

  /**
    * Base implementation just evaluates each point independently.
    * @param points
    * @return
    */
  def applyBatchArea(imgPtArea: ImgPtArea[I, M]): M[PureImgPoints[I]] = {
    val trans_img_pts = imgPtArea.imgPoints map applyToImgPt
    (trans_img_pts map { _.bubbleUpMonad}).toVector.sequence.widen
  }

  //  def applyBatchArea(imgPtArea: ImgPtArea[I, M]): ImgPoints[I, M] = imgPtArea.imgPoints

//  def applyBatch(pts: Points): M[scala.collection.Seq[I]] =
//    ImgTrans.widen[scala.collection.immutable.Vector[I], scala.collection.Seq[I], M]((pts map apply).toVector.sequence)

  def applyBatch(pts: Points): M[Values[I]] = for {
    img_points <- applyBatchArea(imgpt.QuickPtArea(pts.map(LandImgPoint(this, _)), WholeArea()))
  } yield img_points map { _.color }

  def applyM(ptM: M[Point]) = for {
    pt <- ptM
    value <- apply(pt)
  } yield value

  def applyToImgPt(ip: ImgPoint[I, M]) = if (ip.land) {
    InstantImgPoint(ip.point, applyM(ip.point), true)
  } else {
    ip
  }
}
