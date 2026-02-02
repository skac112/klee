package com.github.skac112.klee.transtrans

import cats.Monad
import com.github.skac112.klee.{Img, ImgTrans}

trait ImgTransTrans[M[_], T <: ImgTrans[M]] extends (T => T) {
//  def apply(img: T)(using m: Monad[M]): T
}
