package com.github.skac112.klee.flows

import cats.Monad
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.Point

/**
 * Flow (continuous dynamical system) representing points evolution in time: x -> f(x, t)
 **/
trait Flow[M[_]] extends ((Point, Double) => M[Point]) {
  implicit val m: Monad[M]
  /**
    * Time map - map from point to point for given time. Default implementation just curries apply function
    * but descendant implementation can do improvements.
    * @param time
    * @return
    */
  def timeMap(time: Double): VectorMap[M] = new VectorMap[M] {
    override implicit val m = Flow.this.m
    override def apply(p: Point) = Flow.this.apply(p, time)
  }
}
