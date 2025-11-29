package com.github.skac112.klee

import cats.Monad
import cats.data.Kleisli
import cats.implicits.*
import com.github.skac112.vgutils.ColorVector

object ImgTrans:
  given imgToImgTrans[M[_] : Monad]: Conversion[Img[M], ImgTrans[M]] = (img: Img[M]) => new ImgTrans[M] {
    override def apply(dummy: Img[M])(implicit m: Monad[M]) = img
  }

  def id[M[_]: Monad]: ImgTrans[M] = new ImgTrans[M] {
    override def apply(img: Img[M])(implicit m: Monad[M]) = img
  }

  def widen[N, W, M[_]: Monad](ma: M[N])(implicit ev: N <:< W): M[W] = ma.flatMap[W](img_val => implicitly[Monad[M]].pure(img_val))

abstract class ImgTrans[M[_]]:
  def apply(img: Img[M])(implicit m: Monad[M]): Img[M]
  /**
    * Widens type of output monad.
    * @param ma
    * @param ev
    * @tparam W
    * @tparam M
    * @return
    */
  def widen[W](ma: M[ColorVector])(implicit ev: ColorVector <:< W, m: Monad[M]): M[W] = ma.flatMap[W](img_val => implicitly[Monad[M]].pure(img_val))
