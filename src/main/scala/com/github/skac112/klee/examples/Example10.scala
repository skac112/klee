package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFile, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import com.github.skac112.vgutils._
import scala.math._
import com.github.skac112.klee.transforms.areas.Ring

class Example10 {
  val rand = new scala.util.Random(0)
  val c = Point(.5, .5)
  val init_img = Fill[Color, Id](Color.white)

  val rings_num = 3
  val rings = for {
    i <- 0 to rings_num
    angle = Angle(i * 2 * Pi / rings_num)
    r = .25
    d = .15
  } yield Ring[Color, Color, Id](c + Point.withAngle(angle, d), r -.03, r + .03, Color.hsla(angle, .8, .3, 1.0))  

  val bhs_num = 3
  val bhs = for {
    i <- 0 to bhs_num
    val rot = 15*(1 - 2 * rand.nextDouble) * Pi
    val rot_decay = 8.0 + 10.0 * rand.nextDouble()
  } yield BlackHole[Color, Id](Point(rand.nextDouble(), rand.nextDouble()), rot, rot_decay, 1, 0, 1)

  val fun = Composition[Color, Id](rings.toList ::: bhs.toList)
//   val fun = Composition[Color, Id](circles.toList)
  drawToFile[Color, Id](fun(init_img), trivialColorFun, "example10_1.png", 0, 1, 0, 1, 1500, 1500)
}
