package com.github.skac112.klee.transforms.bundles

import cats.Monad
import cats.implicits._
import cats._
import com.github.skac112.klee.transforms.bundles.Bundle._
import com.github.skac112.vgutils._
import com.github.skac112.vgutils.Point._

object SectionBundle:
//  type Curve[M[_]] = Double => M[Point]
//  type ColoredCurve[I, M[_]] = Double => M[(Point, I)]

  trait ColoredSection[I, M[_]]:
    def angle: Angle
    def halfLength: Double
    def colorDensity(y: Double): (I, Double)
    def blendFun: (srcColor: I, srcColorAmount: Double, sectionColor: I, sectionColorAmount: Double) => M[I]

import SectionBundle._

/**
  * Bundle build from colored line section moving along trajectory.
  * @tparam I
  * @tparam M
  */
trait SectionBundle[I, M[_]: Monad] extends Bundle[I, M]:
  def backbone(d: Double): M[Point]
  def section(d: Double): ColoredSection[I, M]

  override def bundle(pt: Point)(using Monad[M]): (M[Point], I, Double, BlendFun[I, M]) =
    val sec = section(pt.x)
    // x coordinate of in point (pt) spans length direction of band
    val trans_pt = for {
      traj_pt <- backbone(pt.x)
    } yield traj_pt + Point.withAngle(sec.angle, sec.halfLength)

    val color_density = sec.colorDensity(pt.y)
    (trans_pt, color_density._1, color_density._2, sec.blendFun)


