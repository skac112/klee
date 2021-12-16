package com.github.skac112.klee.flows

import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.Point

/**
 * Flow (continuous dynamical system) representing points evolution in time: x -> f(x, t)
 **/
trait Flow extends ((Point, Double) => Point) {
  /**
    * Time map - map from point to point for given time. Default implementation just curries apply function
    * but descendant implementation can do improvements.
    * @param time
    * @return
    */
  def timeMap(time: Double): VectorMap = (p: Point) => apply(p, time)
}
