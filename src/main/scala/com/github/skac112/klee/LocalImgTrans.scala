package com.github.skac112.klee

import cats._
import cats.Monad
import cats.implicits._
import com.github.skac112.klee.ImgTrans.imgToImgTrans
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.imgpt.ImgPointArea.JoinFun
import com.github.skac112.klee.area.pt.PtArea
import com.github.skac112.vgutils._
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._
import com.github.skac112.klee.area.imgpt.ImgPtArea

/**
  * ImgTrans which changes only a part of an image leaving the rest unmodified.
  */
abstract class LocalImgTrans[I, M[_]: Monad] extends ImgTrans[I, I, M] {
  self =>
  def area: ImgArea

  val m: Monad[M] = implicitly[Monad[M]]

  def apply(img: Img[I, M]) = new Img[I, M] {
    override val m: Monad[M] = implicitly[Monad[M]]

    override def apply(p: Point) = if (area contains p) {
      applyInArea(img, p) }
    else {
      // point outside ImgTrans area - bypassing
      img(p)
    }

    override def applyBatchArea(ptArea: ImgPtArea[I, M]) = applyToImgPtArea(img, ptArea)
    override def air = applyToAir(img)
  }

//  protected def makeImgPoints(points: Points, colors: (Int) => () => M[I], land: Boolean = true): ImgPoints[I, M] =
//    points.zipWithIndex map { kv => LazyImgPoint(kv._1, colors(kv._2), land) }

  protected def applyToImgPtArea(img: Img[I, M], ptArea: ImgPtArea[I, M]): M[PureImgPoints[I]] = for {
    part <- ptArea.partByIntersect[I](area): M[(ImgPtArea[I, M], ImgPtArea[I, M], ImgPtArea[I, M], JoinFun[I, M])]
    in = part._1
    out = part._2
    unknown = part._3
    join_fun = part._4
//    (in, out, unknown, join_fun) <- ptArea.partByIntersect[I](area): M[(ImgPtArea[I, M], ImgPtArea[I, M], ImgPtArea[I, M], JoinFun[I])]
    // points transformed by this trans - inside trans area
    in_colors = applyBatchInArea(img, in.imgPoints)
    // point taken from input image - outside trans area
    out_colors = img.applyBatchArea(out)
    // colors for unknown area (colors themselves are 'known')
    unknown_colors = unknown.imgPoints.toVector.par map { (ip: ImgPoint[I, M]) => for {
      pt <- ip.point
      new_img_pt <- for {
        pt <- ip.point
        new_ip <- if (area contains pt) applyInArea(img, ip) else ip
      } yield new_ip
    }}
  } yield join_fun(in_colors, out_colors, unknown_colors)

  def applyToAir(img: Img[I, M]): ImgPoints[I, M] = img.air

  def applyInArea(img: Img[I, M], p: Point): M[I] = applyInArea(img, LandImgPoint(img, p)).color

  def applyInArea(img: Img[I, M], ip: ImgPoint[I, M]): ImgPoint[I, M]

//  def applyBatchInArea(img: Img[I, M], points: Points): M[scala.collection.Seq[O]] =
//    ImgTrans.widen[scala.collection.immutable.Vector[O], scala.collection.Seq[O], M]((points.toVector.par map { applyInArea(img, _) }).toVector.sequence)

  def applyBatchInArea(img: Img[I, M], imgPoints: ImgPoints[I, M]): ImgPoints[I, M] =
    (imgPoints.par map { applyInArea(img, _) }).seq
}