package com.github.skac112.klee.paintrunners

import cats.Id
import com.github.skac112.klee.painters.FingerSun
import com.github.skac112.klee.painters.FingerSun.FingerSunParams
import com.github.skac112.klee.painters.Painter.RenderParams
import com.github.skac112.vgutils.{Bounds, Point}

case class FingerSunPaintRunner(randSeed: Int = 0) extends InterruptibleRunner[Id] {
  override lazy val paints = paintsStream
  lazy val rand = new scala.util.Random(randSeed)
  lazy val paintsStream: LazyList[() => Unit] = newPaint #:: paintsStream

  def newPaint = () => {
    val params = FingerSunParams(numFinger = 40)

    val render_params = RenderParams(
      Bounds(Point(-.5, -.5), Point(.5, .5)),
      nx = 500,
      ny = 500)

    val painter = new FingerSun(params, render_params)
    painter.paint()
  }
}
