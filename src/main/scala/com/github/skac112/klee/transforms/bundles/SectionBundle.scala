package com.github.skac112.klee.transforms.bundles

import cats.Monad
import cats.implicits._
import cats._
import com.github.skac112.klee.transforms.bundles.Bundle.{*, given}
import com.github.skac112.vgutils._
import com.github.skac112.vgutils.Point._
import scala.math.{*, given}

object SectionBundle:
  trait ColoredSection[M[_]]:
    def angle: Angle
    def halfLength: Double
    def blendFun(d: Double): BlendFun[M]

import SectionBundle._

/**
  * Bundle build from colored line section moving along trajectory.
  * @tparam I
  * @tparam M
  */
trait SectionBundle[M[_]: Monad] extends Bundle[M]:
  def backbone(d: Double): M[Point]
  def section(d: Double): ColoredSection[M]

  override def bundle(pt: Point)(using Monad[M]): (M[Point], BlendFun[M]) =
    val sec = section(pt.x)
    // wersor pochodnej backbone dla d = pt.x
    val backbone_dir: Point = ???
    // x coordinate of in point (pt) spans length direction of band
    val trans_pt = for {
      traj_pt <- backbone(pt.x)
    } yield traj_pt + Point.withAngle(backbone_dir.angle + sec.angle + .5*Pi, sec.halfLength) * pt.y
    (trans_pt, sec.blendFun(pt.y))