package com.github.skac112.klee

import cats.Monad
import cats.data.Kleisli
import cats.implicits._

object ImgTrans {
  implicit def imgToImgTrans[I, M[_] : Monad](img: Img[I, M]) = new ImgTrans[I, I, M] {
    override def apply(dummy: Img[I, M])(implicit m: Monad[M]) = img
  }

  def id[I, M[_]: Monad]: ImgTrans[I, I, M] = new ImgTrans[I, I, M] {
    override def apply(img: Img[I, M])(implicit m: Monad[M]) = img
  }

  /**
    * Image transformation where input and output images are of the same type.
    * @tparam I
    * @tparam M
    */
  type Simple[I, M[_]] = ImgTrans[I, I, M]

  def widen[N, W, M[_]: Monad](ma: M[N])(implicit ev: N <:< W): M[W] = ma.flatMap[W](img_val => implicitly[Monad[M]].pure(img_val))
}

abstract class ImgTrans[I, O, M[_]] {

  def apply(img: Img[I, M])(implicit m: Monad[M]): Img[O, M]
  /**
    * Widens type of output monad.
    * @param ma
    * @param ev
    * @tparam W
    * @tparam M
    * @return
    */
  def widen[W](ma: M[O])(implicit ev: O <:< W, m: Monad[M]): M[W] = ma.flatMap[W](img_val => implicitly[Monad[M]].pure(img_val))
}