package com.github.skac112.klee

object Composition {
}

case class Composition(elements: Seq[ImgTrans]) extends ImgTrans {
  lazy val fun: ImgTrans = elements.foldLeft(identity) {(current, element) => current.andThen(element)}
  def apply(img: Img) = fun(img)

  def this(element: ImgTrans, times: Int) = this(Seq.fill[ImgTrans](times)(element))
}
