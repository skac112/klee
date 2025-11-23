package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.*

trait BaseParallelogram extends ImgArea {
  def middle: Point
  def xSide: Double
  def ySide: Double
  def xAngle: Angle
  def yAngle: Angle
}