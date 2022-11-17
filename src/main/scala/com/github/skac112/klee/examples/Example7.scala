package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Ring
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFileOld, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import com.github.skac112.vgutils._
import scala.math._
import com.github.skac112.klee.images.Lines
import com.github.skac112.klee.ImgTrans
import com.github.skac112.klee.transforms.areas.Circle

class Example7 {
  val c = Point(.5, .5)
  val init_img = Fill[Color, Id](Color.white)
  val big_circle = Circle[Color, Id](c, .3, Color.red(.7))
  val cc = 4
  val r = .25

  val small_circles = (0 until cc) map {i => 
    Circle[Color, Id](c + Point.withAngle(Angle(i * 2.0 * Pi / cc), r + i / 30), .15, Color.black)
  }

//   val k = .1
//   val bh = BlackHole[Color, Id](c + Point(k, k), 10 * Pi, 7, 1.0, 0, .7)

  val bhc = 5
  val rbh = .25
  val bhs = for {
    i <- 0 until bhc        
    angle = i * 2.0 * Pi / bhc + 1 
    rot1 = 6*Pi 
    rot = if (i % 2 == 1) rot1 else -rot1       
  } yield BlackHole[Color, Id](c + Point.withAngle(Angle(angle), rbh), rot, 15.0, 1, 0, .2)
  
  val comp = Composition(big_circle :: small_circles.toList ::: bhs.toList)
  drawToFileOld[Color, Id](comp(init_img), trivialColorFun, "example7_26.png", 0, 1, 0, 1, 1500, 1500)
}
