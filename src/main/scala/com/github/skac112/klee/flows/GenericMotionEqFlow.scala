package com.github.skac112.klee.flows

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.Point

/**
  * FLow (continouous dynamical system) determined by it's motion equation, i.e. velocity vector field.
  * @param motionEq
  * @param h step parameter
  */
abstract class GenericMotionEqFlow[M[_]] extends Flow[M] {
  /**
    * Gives velocity i.e. time derivative of a "moving" point for given point.
    * @param p
    * @return
    */
  val motionEq: VectorMap[M]
  def h: Double

  /**
    * Calculates image of point p in transformation induced by dynamical system for time t. Uses fourth order
    * Runge-Kutta method.
    * @param point
    * @param time
    * @return
    */
  override def apply(point: Point, time: Double)(implicit m: Monad[M]): M[Point] = {
    // for negative time actual h must be also negative
    val act_h = h * math.signum(time)
    val steps = math.round(time / act_h).toInt
    val zero_move: M[Point] = VectorMap.identity[M].apply(point)
    (0 until steps).foldLeft(zero_move) { (p: M[Point], i: Int) => p.flatMap(rungeKutta4(motionEq, _, act_h)) }
  }

  override def timeMap(time: Double)(implicit m: Monad[M]): VectorMap[M] = {
    // for negative time actual h must be also negative
    val act_h = h * math.signum(time)
    val steps = math.round(time / act_h).toInt
    val atomic_time_map: VectorMap[M] = VectorMap.identity[M].rungeKutta4(motionEq, act_h)
    (0 until steps).foldLeft(VectorMap.identity[M]) { case (time_map: VectorMap[M], i) =>
      time_map.andThen(atomic_time_map) }
  }
}
