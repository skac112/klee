package com.github.skac112.klee.transforms.bundles

import com.github.skac112.klee.transforms.bundles.Bundle.BlendFun
import com.github.skac112.vgutils.{Angle, Point}

object SectionBundle:
  type Curve[M[_]] = Double => M[Point]
//  type ColoredCurve[I, M[_]] = Double => M[(Point, I)]

  trait ColoredSection[I, M[_]]:
    def angle: Angle
    def halfLength: Double
    def colorDensityFun: Double => (I, Double)
    def blendFun(srcColor: I, srcColorAmount: Double, sectionColor: I, sectionColorAmount: Double): M[I]

import SectionBundle._

/**
  * Bundle build from colored line section moving along trajectory.
  * @tparam I
  * @tparam M
  */
trait SectionBundle[I, M[_]] extends Bundle[I, M]:
  def trajectory: Curve[M]
  def sectionFun(d: Double): ColoredSection[I, M]
  def bundleFun: Point => (M[Point], I, BlendFun[I, M]) = ???

