package com.github.skac112.klee.area.imgpt

import cats.Monad

case class EmptyArea[M[_]: Monad]() extends ImgPtArea[M] {
  override lazy val imgPoints = Seq()
  override lazy val area = com.github.skac112.klee.area.img.EmptyArea()
}