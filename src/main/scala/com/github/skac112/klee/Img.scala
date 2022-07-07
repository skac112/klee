package com.github.skac112.klee

import com.github.skac112.klee.area.img.WholeArea
import com.github.skac112.klee.area.pt
import com.github.skac112.klee.area.pt.{PtArea, QuickPtArea}
import com.github.skac112.vgutils.Point
import cats.{Applicative, Monad}
import cats.implicits._

trait Img[I, M[_]] extends ((Point) => M[I]) {
  implicit val m: Monad[M]

  /**
    * Base implementation just evaluates each point independently.
    * @param points
    * @return
    */
  def applyBatchArea(ptArea: PtArea): M[Seq[I]] = (ptArea.points map apply).toList.sequence.widen[Seq[I]]

  def applyBatch(pts: Points): M[Seq[I]] = applyBatchArea(pt.QuickPtArea(pts, WholeArea()))
}
