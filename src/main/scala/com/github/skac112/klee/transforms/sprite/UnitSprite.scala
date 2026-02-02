package com.github.skac112.klee.transforms.sprite

import com.github.skac112.klee.area.img.Rect
import com.github.skac112.vgutils.{Angle, Point}

trait UnitSprite[M[_]] extends SimpleSprite[M]:
  override def spriteArea = Rect(Point(0.5, 0.5), 1.0, 1.0, Angle(0.0))