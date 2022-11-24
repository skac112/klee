package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.transform.PivotRot
import scala.math._

case class Finger[I, M[_]: Monad](from: Point, to: Point, frontDecay: Double, sideDecay: Double)
  extends Displacer[I, M] {
  override def displacement: VectorMap[M] = new VectorMap[M] {
    // front vector
    lazy val frontVector = (from - to)
    // front versor
    lazy val frontVersor = frontVector.versor
    // side versor - perpendicular to front vector
    lazy val sideVersor = frontVersor.rot(PivotRot(.5*Pi))

    override def apply(p: Point) = m.pure({
      // relative vector - vector from "to" point to point "p"
      val relVec = p - to
      val relVersor = relVec.versor
      val front_factor = relVersor * frontVersor
      val side_factor = relVersor * sideVersor
      // for determining an actual decay factor for point "p", the proportion of two above calculated factors and
      // values of frontDecay and sideDecay are taken into account. Actual decay factor will be something between these
      // two decay factors with obvious special cases when one of the moduli is 0.0. When both moduli are 0.0
      // below a square of decay factor is calculated
      val decay_factor_2 = (front_factor, side_factor) match {
        case (0.0, 0.0) => .25 * (frontDecay + sideDecay) * (frontDecay + sideDecay)
        case _ => front_factor*front_factor*frontDecay + side_factor*side_factor*sideDecay
      }
      frontVector * exp(-.5*relVec.modulus2*decay_factor_2)
    })
  }
}
