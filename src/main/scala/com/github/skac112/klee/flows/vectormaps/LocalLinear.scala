package com.github.skac112.klee.flows.vectormaps

import cats.Monad
import com.github.skac112.vgutils.Point
import com.github.skac112.vgutils.transform.linear.GenericLinear

case class LocalLinear[M[_]](linear: GenericLinear, center: Point, decay: Double) extends VectorMap[M] {
//  override val m = implicitly(Monad[M])

  override def apply(p: Point)(implicit m: Monad[M]) = {
    val p_loc = p - center
    implicitly(Monad[M]).pure(p + (linear(p_loc) * math.exp(-decay*(p_loc.modulus))))
  }
}
