package com.github.skac112.klee.dynsys

import com.github.skac112.klee.dynsys.vectormaps.VectorMap
import com.github.skac112.vgutils.Point

/**
 * Dynamical system representing points evolution in time: x -> f(x, t)
 **/
trait DynamicalSystem extends ((Point, Double) => Point) {
  /**
    * Time map - map from point to point for given time. Default implementation just curries apply function but descendant implementation can
    * do improvements.
    * @param time
    * @return
    */
  def timeMap(time: Double): VectorMap = (p: Point) => apply(p, time)
}
