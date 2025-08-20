package com.github.skac112.klee

import cats.*
import cats.Monad
import cats.implicits.*
import com.github.skac112.vgutils.{ColorVector, Point}

sealed trait PureImgPoint {
  def point: Point
  def color: ColorVector
  def land: Boolean = false
}

final case class InstantPureImgPoint(override val point: Point, color: ColorVector, override val land: Boolean = true) extends PureImgPoint
