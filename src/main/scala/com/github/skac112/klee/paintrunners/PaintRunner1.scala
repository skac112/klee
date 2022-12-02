package com.github.skac112.klee.paintrunners

import com.github.skac112.klee.painters.Painter.RenderParams
import com.github.skac112.klee.painters.Painter1
import com.github.skac112.klee.painters.Painter1.Painter1Params
import com.github.skac112.vgutils.{Bounds, Point}

case class PaintRunner1() extends PaintRunner {
  val params = Painter1Params(randSeed = 2,
    numElems = 500,
    aBase = 10.0,
    aSpan = 40.0,
    colorDisperse = .8)

  val render_params = RenderParams(
    Bounds(Point(0, 0), Point(1, 1)),
    nx = 1000,
    ny = 1000)

  val painter = new Painter1(params, render_params)
  painter.paint()
}
