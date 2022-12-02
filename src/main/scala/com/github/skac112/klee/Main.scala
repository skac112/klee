package com.github.skac112.klee

import cats.Id
import com.github.skac112.klee.examples._
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.linalg2d.spiral
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.vgutils.{Bounds, Color, Point}
import com.github.skac112.klee.painters._
import com.github.skac112.klee.painters.Painter.RenderParams
import com.github.skac112.klee.painters.Painter1.Painter1Params
import com.github.skac112.klee.paintrunners._

object Main extends App {
    PaintRunner3()
}
