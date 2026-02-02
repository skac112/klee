package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.{Angle, Point}

case class QuickParallelogram(middle: Point, xSideLen: Double, ySideLen: Double, xAngle: Angle, yAngle: Angle) extends BaseParallelogram