package com.github.skac112.klee

import cats._
import cats.Monad
import cats.implicits._
import com.github.skac112.vgutils.Point

sealed trait PureImgPoint[+I] {
  def point: Point
  def color: I
  def land: Boolean = false
}

final case class InstantPureImgPoint[+I](override val point: Point, color: I, override val land: Boolean = true) extends PureImgPoint[I]
