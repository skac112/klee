package com.github.skac112.klee

import com.github.skac112.vgutils.{MathUtils, Point}

package object dynsys {
  val f1_6 = 1.0 / 6.0

  /**
    * Dynamical system representing points evolution in time:
    * x -> f(x, t)
    */
  type DynamicalSystem = (Point, Double) => Point

  /**
    * Fourth order Runge-Kutta method for points (function fun is a time-derivative of a point p):
    * p' = fun(p) = dp/dt(p).
    * @param fun
    * @param p
    * @param h
    * @return
    */
  def rungeKutta4(fun: Point => Point, p: Point, h: Double) = {
    val k1 = fun(p) * h
    val k2 = fun(p + (k1 * .5)) * h
    val k3 = fun(p + (k2 * .5)) * h
    val k4 = fun(p + k3) * h
    p + (k1 + (k2 * 2) + (k3 * 2) + k4) * f1_6
  }

}
