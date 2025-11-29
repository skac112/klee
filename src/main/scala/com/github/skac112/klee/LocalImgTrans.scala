package com.github.skac112.klee

import cats.*
import cats.Monad
import cats.implicits.*
import com.github.skac112.klee.ImgTrans.imgToImgTrans
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.imgpt.ImgPointArea.JoinFun
import com.github.skac112.klee.area.pt.PtArea
import com.github.skac112.vgutils.*

import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters.*
import com.github.skac112.klee.area.imgpt.ImgPtArea
import com.github.skac112.klee.images.MutableRaster

/**
  * ImgTrans which changes only a part of an image leaving the rest unmodified.
  */
abstract class LocalImgTrans[M[_]] extends ImgTrans[M] {
  self =>
  def area: ImgArea

//  lazy val m: Monad[M] = implicitly[Monad[M]]

  override def apply(img: Img[M])(using m: Monad[M]) = img match
    case mutable_raster @ MutableRaster(width, height, initImg, interpolation) =>
      val img_pts = mutable_raster.imgPoints
      applyBatchInArea(mutable_raster, img_pts).map { new_img_pts =>
        new_img_pts.foreach { ip =>
          mutable_raster.updatePixel(ip.point.x.floor.toInt, ip.point.y.floor.toInt, ip.color)
        }
      }
      mutable_raster
      
    case _ => new Img[M] {
      override def apply(p: Point)(using m: Monad[M]) = if (area contains p) {
        applyInArea(img, p)
      }
      else {
        // point outside ImgTrans area - bypassing
        img(p)
      }

      override def applyBatchArea(ptArea: ImgPtArea[M])(using m: Monad[M]) = applyToImgPtArea(img, ptArea)

      override def points(using m: Monad[M]) = applyToAir(img)
    }

//  protected def makeImgPoints(points: Points, colors: (Int) => () => M, land: Boolean = true): ImgPoints[M] =
//    points.zipWithIndex map { kv => LazyImgPoint(kv._1, colors(kv._2), land) }

  /**
    * Applies this ImgTrans to given image area.
    * @param img
    * @param ptArea
    * @return
    */
  protected def applyToImgPtArea(img: Img[M], ptArea: ImgPtArea[M])(using m: Monad[M]): M[PureImgPoints] =
    println("applyToImgPtArea")
    for {
      part <- ptArea.partByIntersect(self.area): M[(ImgPtArea[M], ImgPtArea[M], ImgPtArea[M], JoinFun[M])]
      in = part._1
      out = part._2
      unknown = part._3
      join_fun = part._4
      //    (in, out, unknown, join_fun) <- ptArea.partByIntersect(area): M[(ImgPtArea[M], ImgPtArea[M], ImgPtArea[M], JoinFun)]
      // points transformed by this trans - inside trans area
      in_colors <- applyBatchInArea(img, in.imgPoints)
      // point taken from input image - outside trans area
      out_colors <- img.applyBatchArea(out)
      // colors for unknown area (colors themselves are 'known')
      unknown_colors <- (unknown.imgPoints.toVector.par map { (ip: ImgPoint[M]) => for {
        pt <- ip.point
        new_ip = if (self.area contains pt) applyInArea(img, ip) else img.applyToImgPt(ip)
        pure_img_pt <- new_ip.bubbleUpMonad
      } yield pure_img_pt}).toVector.sequence
    } yield join_fun(in_colors, out_colors, unknown_colors)

  def applyToAir(img: Img[M])(using m: Monad[M]): M[PureImgPoints] = img.points

  def applyInArea(img: Img[M], p: Point)(using m: Monad[M]): M[ColorVector] = applyInArea(img, LandImgPoint(img, p)).color

  def applyInArea(img: Img[M], ip: ImgPoint[M])(using m: Monad[M]): ImgPoint[M]

//  def applyBatchInArea(img: Img[M], points: Points): M[scala.collection.Seq[O]] =
//    ImgTrans.widen[scala.collection.immutable.Vector[O], scala.collection.Seq[O], M]((points.toVector.par map { applyInArea(img, _) }).toVector.sequence)

  def applyBatchInArea(img: Img[M], imgPoints: ImgPoints[M])(using m: Monad[M]): M[PureImgPoints] =
    (imgPoints.par map { applyInArea(img, _).bubbleUpMonad }).seq.toVector.sequence.widen
}