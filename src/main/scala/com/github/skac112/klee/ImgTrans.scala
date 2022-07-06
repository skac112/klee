package com.github.skac112.klee

import cats.Monad
import cats.data.Kleisli
import cats.implicits._

object ImgTrans {
  implicit def imgToImgTrans[I, M[_] : Monad](img: Img[I, M]) = new ImgTrans[I, I, M] {
    override val m = implicitly[Monad[M]]
    override def apply(dummy: Img[I, M]) = img
  }

  def id[I, M[_]: Monad]: ImgTrans[I, I, M] = new ImgTrans[I, I, M] {
    override val m = implicitly[Monad[M]]
    override def apply(img: Img[I, M]) = img
  }

  type Simple[I, M[_]] = ImgTrans[I, I, M]

  def widen[N, W, M[_]: Monad](ma: M[N])(implicit ev: N <:< W): M[W] = ma.flatMap[W](img_val => implicitly[Monad[M]].pure(img_val))
}

trait ImgTrans[I, O, M[_]] extends (Img[I, M] => Img[O, M]) {
  implicit val m: Monad[M]
//  lazy val k: Kleisli[M, Img[I, M], Img[O, M]] = Kleisli(this.apply)
  //    def compose[OI <: Color, OO <: I](other: ImgTrans[OI, OO, M): ImgTrans[]

  /**
    * Widens type of output monad.
    * @param ma
    * @param ev
    * @tparam W
    * @tparam M
    * @return
    */
  def widen[W, M[_] : Monad](ma: M[O])(implicit ev: O <:< W): M[W] = ma.flatMap[W](img_val => implicitly[Monad[M]].pure(img_val))
}