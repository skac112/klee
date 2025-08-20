package com.github.skac112.klee.area.imgpt

import cats.Monad
import com.github.skac112.klee.{ImgPoints, Points}
import com.github.skac112.klee.area.img.ImgArea

case class QuickPtArea[M[_]: Monad](override val imgPoints: ImgPoints[M], override val area: ImgArea) extends ImgPtArea[M]
