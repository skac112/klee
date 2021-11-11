package com.github.skac112.klee

import com.github.skac112.klee.area.img.WholeArea
import com.github.skac112.klee.area.pt
import com.github.skac112.klee.area.pt.{PtArea, QuickPtArea}
import com.github.skac112.vgutils.Point

trait PtAreaBatchable[U] extends Batchable[Point, U] {
  /**
    * Base implementation just evaluates each point independently, but custom implementations
    * can make performance improvements.
    * @param points
    * @return
    */
  def applyBatchArea(ptArea: PtArea): Seq[U] = ptArea.points map apply _
  override def applyBatch(pts: Points): Seq[U] = applyBatchArea(pt.QuickPtArea(pts, WholeArea()))
}
