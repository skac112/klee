package com.github.skac112.klee

import cats.Id
import com.github.skac112.klee.examples._
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.linalg2d.spiral
import com.github.skac112.klee.painters.FingerComb.FingerCombParams
import com.github.skac112.klee.painters.FingerSun.FingerSunParams
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.vgutils.{Bounds, Color, Point}
import com.github.skac112.klee.painters._
import com.github.skac112.klee.painters.Painter.RenderParams
import com.github.skac112.klee.painters.Painter1.Painter1Params
import com.github.skac112.klee.paintrunners._

object Main extends App {
    val params = FingerSunParams(numFinger = 150)

    val render_params = RenderParams(
        Bounds(Point(-.5, -.5), Point(.5, .5)),
        nx = 1000,
        ny = 1000)

    val painter = new FingerSun(params, render_params)
    painter.paint()
}
