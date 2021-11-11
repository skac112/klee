package com.github.skac112.klee.area.img

import com.github.skac112.vgutils.{Bounds, Point}

/**
  * Area of image of a rectangular shape. Sides of the rectangle have directions of coordinate axes.
  *
  * @param leftTop
  * @param width
  * @param height
  */
case class AxisRect(leftTop: Point, width: Double, height: Double) extends ImgArea {
  override def contains(p: Point): Boolean = p.x >= leftTop.x && p.y <= leftTop.x + width && p.y >= leftTop.y &&
    p.y <= leftTop.y + height

  override def bounds = Some(Bounds(leftTop, leftTop + Point(width, height)))
}
