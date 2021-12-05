package com.github.skac112.klee

import com.github.skac112.klee.examples._
import com.github.skac112.klee.linalg2d.spiral

object Main extends App {
//  new PolyMapExample
//  new Example2
  val alfa = -.3
  val beta = 1.5
  val tau = .25*math.Pi
  val axis_ratio = 6.0
  val m = spiral(alfa, beta, tau, axis_ratio)
  println(m.a)
  println(m.b)
  println(m.c)
  println(m.d)
}
