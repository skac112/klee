package com.github.skac112.klee

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.ImgTrans.imgToImgTrans
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.pt.PtArea
import com.github.skac112.vgutils._
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.CollectionConverters._

/**
  * ImgTrans which changes only a part of an image leaving the rest unmodified.
  */
abstract class LocalImgTrans[I <: O, O, M[_]: Monad] extends ImgTrans[I, O, M] {
  self =>
//  implicit val ev: I <:< O
//  implicit val evSeq: Seq[I] <:< Seq[O]
  def area: ImgArea

  def apply(img: Img[I, M]) = new Img[O, M] {
    override val m: Monad[M] = implicitly[Monad[M]]

    override def apply(p: Point) = if (area contains p) {
      applyInArea(img, p) }
    else {
      // point outside ImgTrans area - bypassing
      ImgTrans.widen[I, O, M](img(p))
    }

    override def applyBatchArea(ptArea: PtArea): M[scala.collection.Seq[O]] = {    
        val (in, out, unknown, join_fun) = ptArea.partByIntersect[O](area)
       
        // points transformed by this trans - inside trans area
        for {
            in_colors <- applyBatchInArea(img, in.points)
            // point taken from input image - outside trans area
            out_colors <- img.applyBatchArea(out)                
            // colors for unknown area (colors themselves are 'known')
            unknown_colors <- (unknown.points.toVector.par map { p =>                 
                if (area contains p) {
                    applyInArea(img, p)
                }
                else {
                    ImgTrans.widen[I, O, M](img(p))
                }
            }).toVector.sequence       
        } yield join_fun(in_colors, out_colors, unknown_colors)
    }

    override def air = applyToAir(img)
  }

  def applyToAir(img: Img[I, M]): Seq[Droplet[O, M]] = img.air.asInstanceOf[Seq[Droplet[O, M]]]

  def applyInArea(img: Img[I, M], p: Point): M[O]

  def applyBatchInArea(img: Img[I, M], points: Points): M[scala.collection.Seq[O]] = 
    // ImgTrans.widen[scala.collection.immutable.Vector[O], scala.collection.Seq[O], M]((points map { applyInArea(img, _) }).toVector.sequence)
    ImgTrans.widen[scala.collection.immutable.Vector[O], scala.collection.Seq[O], M]((points.toVector.par map { applyInArea(img, _) }).toVector.sequence)
}