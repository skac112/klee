package com.github.skac112.klee

import com.github.skac112.vgutils.Point

object Composition {
  def compose[T](elementFun: Int => ImgTrans[T], times: Int) = this((0 until times) map {i => elementFun(i)})
}

/**
  * Composition of ImgTrans. First ImgTrans in elements sequence is the innermost (applied first).
  * @param elements
  */
case class Composition[T](elements: Seq[ImgTrans[T]]) extends ImgTrans[T] {
  lazy val fun: ImgTrans[T] = elements.foldLeft(identity[T]) {(current, element) => current.andThen(element)}

  def apply(img: Img[T]) = fun(img)

//    new Img {
//    override def apply(p: Point) = fun(img)(p)
//    override def applyBatch(p: Points) =
//  }

  def this(element: ImgTrans[T], times: Int) = this(Seq.fill[ImgTrans[T]](times)(element))
}
