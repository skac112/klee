package com.github.skac112.klee.transcomb

import cats.Monad
import com.github.skac112.klee.{Img, ImgTrans}

object Composition {

  case class Params[I, M[_]](elements: scala.collection.Seq[ImgTrans.Simple[I, M]])

  def compose[I, M[_]: Monad](elementFun: Int => ImgTrans.Simple[I, M], times: Int): Composition[I, M] =
    this((0 until times) map {i => elementFun(i)})
}

/**
  * Composition of ImgTrans-es. First ImgTrans in elements sequence is the innermost (applied first).
  * @param elements
  */
case class Composition[I, M[_]](elements: scala.collection.Seq[ImgTrans.Simple[I, M]]) extends ImgTrans.Simple[I, M] {

  lazy val fun: ImgTrans[I, I, M] = elements.reduce { (acc, element) => new ImgTrans[I, I, M] {
      override def apply(img: Img[I, M])(implicit m: Monad[M]): Img[I, M] = element(acc(img))}}

  def apply(img: Img[I, M])(implicit m: Monad[M]): Img[I, M] = fun(img)
  def this(element: ImgTrans.Simple[I, M], times: Int) = this(Seq.fill[ImgTrans.Simple[I, M]](times)(element))
}

