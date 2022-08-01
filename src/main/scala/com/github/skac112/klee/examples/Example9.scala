package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFile, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import com.github.skac112.vgutils._

import scala.math._

class Example9 {
  val c = Point(.5, .5)
  val init_img = Fill[Color, Id](Color.white)
  val r = .2
  val circle = Circle[Color, Color, Id](c, .2, Color.red(.7))
  val bhc = 3

  val bhs = for {
    i <- 0 until bhc        
    angle = i * 2.0 * Pi / bhc 
    rot = 10 * Pi 
    // rot = if (i % 2 == 1) rot1 else -rot1       
  } yield BlackHole[Color, Id](c + Point.withAngle(Angle(angle), r), rot, 15.0, 1, 0, 1)

  val fun = Composition[Color, Id](circle :: bhs.toList)
  drawToFile[Color, Id](fun(init_img), trivialColorFun, "example9_8.png", 0, 1, 0, 1, 1500, 1500)
}
