package com.github.skac112.klee

import com.github.skac112.klee.area.img.WholeArea
import com.github.skac112.klee.area.imgpt
import com.github.skac112.klee.area.pt.{PtArea, QuickPtArea}
import com.github.skac112.vgutils.{ColorVector, Point}
import cats.{Applicative, Monad}
import cats.implicits.*
import com.github.skac112.klee.area.imgpt.ImgPtArea

trait Img[M[_]] {
  def apply(p: Point)(using m: Monad[M]): M[ColorVector]
  def points(implicit m: Monad[M]): M[PureImgPoints] = m.pure(Seq())

  /**
    * Base implementation just evaluates each point independently.
    * @param points
    * @return
    */
  def applyBatchArea(imgPtArea: ImgPtArea[M])(implicit m: Monad[M]): M[PureImgPoints] = {
    val trans_img_pts = imgPtArea.imgPoints map applyToImgPt
    (trans_img_pts map { _.bubbleUpMonad}).toVector.sequence.widen
  }

  //  def applyBatchArea(imgPtArea: ImgPtArea[M]): ImgPoints[M] = imgPtArea.imgPoints

//  def applyBatch(pts: Points): M[scala.collection.Seq] =
//    ImgTrans.widen[scala.collection.immutable.Vector, scala.collection.Seq, M]((pts map apply).toVector.sequence)

  def applyBatch(pts: Points)(implicit m: Monad[M]): M[Values[ColorVector]] = for {
    img_points <- applyBatchArea(imgpt.QuickPtArea(pts.map(LandImgPoint(this, _)), WholeArea()))
  } yield img_points map { _.color }

  def applyM(ptM: M[Point])(implicit m: Monad[M]) = for {
    pt <- ptM
    value <- apply(pt)
  } yield value

  def applyToImgPt(ip: ImgPoint[M])(implicit m: Monad[M]) = if (ip.land) {
    InstantImgPoint(ip.point, applyM(ip.point), true)
  } else {
    ip
  }
}
