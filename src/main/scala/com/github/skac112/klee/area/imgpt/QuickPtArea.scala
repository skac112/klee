package com.github.skac112.klee.area.imgpt

import cats.Monad
import com.github.skac112.klee.{ImgPoints, Points}
import com.github.skac112.klee.area.img.ImgArea

case class QuickPtArea[I, M[_]: Monad](override val imgPoints: ImgPoints[I, M], override val area: ImgArea) extends ImgPtArea[I, M]
