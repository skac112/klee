package com.github.skac112.klee

import com.github.skac112.klee.transforms.displacers.Displacer
import cats.Monad
import cats.implicits._
import com.github.skac112.vgutils.ColorVector

/**
  * For given image trans and sequence of initial points generates an image with trivial land part and air comprising
  * of points of subsequent application of trans to initial points. 
  */
final case class AirIterator[I, M[_]: Monad](imgTrans: ImgTrans[I, I, M], pts: Points) {
//    def drawToFile(): Unit =
}
