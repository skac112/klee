package com.github.skac112.klee
import com.github.skac112.vgutils._

object Img {
  implicit def imgToImgTrans[T](img: Img[T]): ImgTrans[T] = (in: Img[T]) => img
}

trait Img[T] extends PtAreaBatchable[T] {
  /**
    * Base implementation just evaluates each point independently, but custom implementations
    * can make performance improvements.
    * @param points
    * @return
    */
}
