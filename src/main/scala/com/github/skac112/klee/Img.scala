package com.github.skac112.klee

import com.github.skac112.klee.area.img.WholeArea
import com.github.skac112.klee.area.pt
import com.github.skac112.klee.area.pt.{PtArea, QuickPtArea}
import com.github.skac112.vgutils.Point
import cats.{Applicative, Monad}
import cats.implicits._

trait Img[O, M[_]] extends ((Point) => M[O]) {
  implicit val m: Monad[M]

  /**
    * Base implementation just evaluates each point independently.
    * @param points
    * @return
    */
  def applyBatchArea(ptArea: PtArea): M[Seq[O]] = (ptArea.points map apply).toList.sequence.widen[Seq[O]]

  def applyBatch(pts: Points): M[Seq[O]] = applyBatchArea(pt.QuickPtArea(pts, WholeArea()))
}
