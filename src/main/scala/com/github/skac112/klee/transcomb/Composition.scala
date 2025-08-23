package com.github.skac112.klee.transcomb

import cats.Monad
import com.github.skac112.klee.{Img, ImgTrans}

object Composition {

  case class Params[M[_]](elements: scala.collection.Seq[ImgTrans[M]])

  def compose[M[_]: Monad](elementFun: Int => ImgTrans[M], times: Int): Composition[M] =
    this((0 until times) map {i => elementFun(i)})
}

/**
  * Composition of ImgTrans-es. First ImgTrans in elements sequence is the innermost (applied first).
  * @param elements
  */
case class Composition[M[_]](elements: scala.collection.Seq[ImgTrans[M]]) extends ImgTrans[M] {

  lazy val fun: ImgTrans[M] = elements.reduce { (acc, element) => new ImgTrans[M] {
      override def apply(img: Img[M])(implicit m: Monad[M]): Img[M] = element(acc(img))}}

  def apply(img: Img[M])(implicit m: Monad[M]): Img[M] = fun(img)
  def this(element: ImgTrans[M], times: Int) = this(Seq.fill[ImgTrans[M]](times)(element))
}

