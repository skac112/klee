package com.github.skac112.klee
import com.github.skac112.vgutils._

object Img {
  implicit def imgToImgTrans(img: Img): ImgTrans = (in: Img) => img
}

trait Img extends PtAreaBatchable[Color] {
  /**
    * Base implementation just evaluates each point independently, but custom implementations
    * can make performance improvements.
    * @param points
    * @return
    */
//  def applyBatch(points: Points): Colors = points map apply _
}
