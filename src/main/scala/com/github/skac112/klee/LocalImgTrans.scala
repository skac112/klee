package com.github.skac112.klee

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.ImgTrans.imgToImgTrans
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.pt.PtArea
import com.github.skac112.vgutils._

/**
  * ImgTrans which changes only a part of an image leaving the rest unmodified.
  */
trait LocalImgTrans[I <: O, O, M[_]] extends ImgTrans[I, O, M] {
  implicit val imgTransM: Monad[M] = m
//  implicit val ev: I <:< O
//  implicit val evSeq: Seq[I] <:< Seq[O]
  def area: ImgArea

  def apply(img: Img[I, M]) = new Img[O, M] {
    override val m = imgTransM
    override def apply(p: Point) = if (area contains p) {
        applyInArea(img, p) }
      else {
        ImgTrans.widen[I, O, M](img(p))
      }

    override def applyBatchArea(ptArea: PtArea): M[Seq[O]] = {
      val (in, out, unknown, merge_fun) = ptArea.partByIntersect[O](area)
      // points transformed by this trans - inside trans area
      for {
        in_colors <- applyBatchInArea(img, in.points)
        // point taken from input image - outside trans area
        out_colors <- img.applyBatchArea(out)
        // colors for unknown area (colors themselves are 'known')
        unknown_colors <- (unknown.points map { p =>
          if (area contains p) {
            applyInArea(img, p)
          }
          else {
            ImgTrans.widen[I, O, M](img(p))
          }}).toList.sequence
      } yield merge_fun(in_colors, out_colors, unknown_colors)
    }
  }

  def applyInArea(img: Img[I, M], p: Point): M[O]

  def applyBatchInArea(img: Img[I, M], points: Points): M[Seq[O]] =
    ImgTrans.widen[List[O], Seq[O], M]((points map { applyInArea(img, _) }).toList.sequence)
}
