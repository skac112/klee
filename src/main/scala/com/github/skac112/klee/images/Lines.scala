package com.github.skac112.klee.images

import com.github.skac112.klee._
import com.github.skac112.vgutils._

/**
  * Image consisting of infinite number of horizontal and vertical straight lines.
  * @param startX base x coordinate (x coordinate of a middle of base vertical line)
  * @param startY base y coordinate (y coordinate of a middle of base horizontal line)
  * @param dx distance between (middles of) vertical lines
  * @param dy distance between (middles of) hotizontal lines
  * @param width width of lines
  * @param color color of lines
  */
case class Lines(
                  baseX: Double = 0,
                  baseY: Double = 0,
                  dx: Double,
                  dy: Double,
                  width: Double,
                  lineColor: Color,
                  backgroundColor: Color) extends Img {
  override def apply(p: Point) = {
    val half_width = .5 * width
    // number of line column where the point lies
    val x_col = (p.x/dx).floor.toInt
    // number of line row where the point lies
    val y_row = (p.y/dy).floor.toInt
    // x within column
    val col_px = p.x - x_col * dx
    // y within row
    val row_py = p.y - y_row * dy
    if (col_px <= half_width || col_px >= dx - half_width || row_py <= half_width || row_py >= dy - half_width) {
      lineColor
    } else {
      backgroundColor
    }
  }
}
