package com.github.skac112.klee.dynsys

import com.github.skac112.vgutils.Point

/**
  * Dynamical system determined by it's motion equation, i.e. velocity vector field.
  * @param motionEq
  * @param h step parameter
  */
abstract class GenericMotionEqDynamicalSystem extends DynamicalSystem {
  /**
    * Gives velocity i.e. time derivative of a "moving" point for given point.
    * @param p
    * @return
    */
  def motionEq(p: Point): Point
  def h: Double

  /**
    * Calculates image of point p in transformation induced by dynamical system for time t. Uses fourth order
    * Runge-Kutta method.
    * @param point
    * @param time
    * @return
    */
  override def apply(point: Point, time: Double): Point = {
    // for negative time actual h must be also negative
    val act_h = h * math.signum(time)
    val steps = math.round(time / act_h).toInt
    (0 until steps).foldLeft(point) {(p: Point, i: Int) => rungeKutta4(motionEq, p, act_h)}
  }
}
