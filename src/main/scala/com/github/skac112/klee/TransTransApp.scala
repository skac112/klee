package com.github.skac112.klee

import com.github.skac112.klee.transtrans.ImgTransTrans
import cats.Monad
import cats.implicits.*

//case class TransTransApp[M[_], T <: ImgTrans[M], TT <: ImgTransTrans[M, T]](transTrans: TT, trans: T) extends T:
//  lazy val resultTrans = transTrans(trans)
//  override def apply(img: Img[M])(using m: Monad[M]): Img[M] = resultTrans.apply(img)