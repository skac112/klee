package com.github.skac112.klee

import com.github.skac112.vgutils.Point

object Composition {
  def compose(elementFun: Int => ImgTrans, times: Int) = this((0 until times) map {i => elementFun(i)})
}

/**
  * Composition of ImgTrans. First ImgTrans in elements sequence is the innermost (applied first).
  * @param elements
  */
case class Composition(elements: Seq[ImgTrans]) extends ImgTrans {
  lazy val fun: ImgTrans = elements.foldLeft(identity) {(current, element) => current.andThen(element)}

  def apply(img: Img) = fun(img)

//    new Img {
//    override def apply(p: Point) = fun(img)(p)
//    override def applyBatch(p: Points) =
//  }

  def this(element: ImgTrans, times: Int) = this(Seq.fill[ImgTrans](times)(element))
}
