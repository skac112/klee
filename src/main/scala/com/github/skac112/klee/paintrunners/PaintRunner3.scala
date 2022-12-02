package com.github.skac112.klee.paintrunners

import cats.Id
import com.github.skac112.klee.nextGaussBounded
import com.github.skac112.klee.painters.Painter.RenderParams
import com.github.skac112.klee.painters.Painter2
import com.github.skac112.klee.painters.Painter2.Painter2Params
import com.github.skac112.klee.transforms.displacers.Finger
import com.github.skac112.vgutils.{Bounds, Point}

case class PaintRunner3(randSeed: Int = 0) extends InterruptibleRunner[Id] {
  override lazy val paints: LazyList[() => Unit] = paintsStream
  lazy val rand = new scala.util.Random(randSeed)
  lazy val paintsStream: LazyList[() => Unit] = newPaint #:: paintsStream

  def newPaint = () => {
    val x_from = nextGaussBounded(rand, 0.08, .12, .1, .02)
    val from = Point(x_from, 0)
    val x_to = nextGaussBounded(rand, .15, .4, .3, .2)
    val to = Point(x_to, 0)
//    val max_decay = Finger.maxFrontDecayFactor(from, to, Finger.DECAY_EXP)
    val front_decay_f = nextGaussBounded(rand, .5, .9, .7, .4)
    val back_decay_f = front_decay_f * nextGaussBounded(rand, 2, 8, 5, 3)
    val side_decay_f = front_decay_f * nextGaussBounded(rand, 3, 20, 10, 5)

    val params = Painter2Params(
      from,
      to,
      front_decay_f,
      back_decay_f,
      side_decay_f,
      Finger.DECAY_LIN_SAT,
      Finger.DECAY_LIN_SAT,
      Finger.DECAY_GAUSS)

    val render_params = RenderParams(
      Bounds(Point(-.5, -.5), Point(.5, .5)),
      nx = 1000,
      ny = 1000)

    val painter = new Painter2(params, render_params)
    painter.paint()
  }
}
