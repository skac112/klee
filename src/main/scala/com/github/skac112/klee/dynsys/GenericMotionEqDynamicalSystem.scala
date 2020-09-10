package com.github.skac112.klee.dynsys

import com.github.skac112.klee.dynsys.vectormaps.VectorMap
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
  val motionEq: VectorMap
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

  override def timeMap(time: Double): VectorMap = {
    // for negative time actual h must be also negative
    val act_h = h * math.signum(time)
    val steps = math.round(time / act_h).toInt
    val p = Point(1.0, 1.0)
    val atomic_time_map: VectorMap = VectorMap.identity.rungeKutta4(motionEq, act_h)
    (0 until steps).foldLeft(VectorMap.identity) { case (time_map: VectorMap, i) =>
      time_map.andThen(atomic_time_map) }
//    (0 until steps).foldLeft((VectorMap.identity, motionEq)) { case ((time_map, motion_eq), i) =>
//      val new_time_map = time_map.rungeKutta4(motion_eq, act_h)
//      val new_motion_eq = motion_eq compose new_time_map
//      println(new_time_map(p))
//      println(new_motion_eq(p))
//      (new_time_map, new_motion_eq)
//    }._1
  }
}
