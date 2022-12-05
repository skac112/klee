package com.github.skac112.klee.transforms

import com.github.skac112.klee.blendColors
import com.github.skac112.vgutils.{Color, ColorVector}

package object gradients {

  type ColorStep = (ColorVector, Double)
  type ColorFun = (Double) => ColorVector

  class MultiColor(steps: Seq[ColorStep]) extends ColorFun {
    def apply(value: Double) = steps.indexWhere(_._2 <= value) match {
      case 0 => steps(0)._1
      case -1 => steps.last._1
      case ceil_step_idx =>
        val floor_step = steps(ceil_step_idx - 1)
        val ceil_step = steps(ceil_step_idx)
        val prop = (value - ceil_step._2) / (floor_step._2 - ceil_step._2)
        blendColors(floor_step._1, ceil_step._1, prop)
    }
  }
}
