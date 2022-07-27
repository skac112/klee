package com.github.skac112.klee

import cats.Id
import com.github.skac112.klee.examples._
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.linalg2d.spiral
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.vgutils.{Color, Point}

object Main extends App {
  new Example7
//  val src_img = Fill[Color, Id](Color.white)
//  val circle = Circle[Color, Color, Id](Point(.5, .5), .25, Color.black)
//  val dst_img = circle(src_img)
//  val n = 100
//  drawToFile[Color, Id](dst_img, trivialColorFun, s"CircleTest.png", 0.0, 1.0, 0.0, 1.0, n, n)

//  new PolyMapExample
//  new Example2
//  val alfa = -.3
//  val beta = 1.5
//  val tau = .25*math.Pi
//  val axis_ratio = 6.0
//  val m = spiral(alfa, beta, tau, axis_ratio)
//  println(m.a)
//  println(m.b)
//  println(m.c)
//  println(m.d)
}
