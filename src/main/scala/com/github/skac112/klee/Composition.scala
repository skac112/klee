package com.github.skac112.klee

import cats.Monad
import cats.data.Kleisli
import com.github.skac112.vgutils.Point
import com.github.skac112.klee.ImgTrans._
import cats.implicits._
import cats.Monad._

object Composition {
  def compose[I, M[_]: Monad](elementFun: Int => ImgTrans.Simple[I, M], times: Int) =
    this((0 until times) map {i => elementFun(i)})
}

/**
  * Composition of ImgTrans-es. First ImgTrans in elements sequence is the innermost (applied first).
  * @param elements
  */
case class Composition[I, M[_]: Monad](elements: scala.collection.Seq[ImgTrans.Simple[I, M]]) extends ImgTrans.Simple[I, M] {

  lazy val fun: ImgTrans[I, I, M] = elements.reduce { (acc, element) => new ImgTrans[I, I, M] {
      override def apply(img: Img[I, M]) = acc.andThen(element).apply(img)}
  }

  def apply(img: Img[I, M]) = fun(img)
  def this(element: ImgTrans.Simple[I, M], times: Int) = this(Seq.fill[ImgTrans.Simple[I, M]](times)(element))
}
