package com.github.skac112.klee

import breeze.math.Complex
import com.github.skac112.vgutils.Angle
import com.github.skac112.vgutils.transform.linear.{GenericLinear, Linear, Rotation, Scale, UniScale}

import scala.math.sqrt

package object linalg2d {
  implicit def doubleToComplex(d: Double) = Complex(d, .0)

  implicit def genericLinearToComplexMatrix(linear: GenericLinear) =
    ComplexMatrix(linear.a, linear.b, linear.c, linear.d)

  /**
    * Synthesizes 2d-matrix of a spiral node linear dynamical system. Eigenvalues  are complex conjugate and have form:
    * alfa + i*beta
    * where i is imaginary unit
    * Tau is an angle of large axis of "rotation ellipse". "Rotation ellipse" is an ellipse, a phase portrait of any
    * nonzero point of dynamical system with eigenvalues being purely imaginary with absolute values equal to modulus of
    * each of (by definition equal) eigenvalues given (specified by alfa and beta parameters).
    * lambda1 = alfa + i*beta
    * lambda2 = alfa - i*beta
    * @param alfa
    * @param beta
    * @param tau
    * @param axisRatio
    * @return
    */
  def spiral(alfa: Double, beta: Double, tau: Angle, axisRatio: Double): GenericLinear = {
    // matrix of (+/-)Pi/2 rotation and scaling by beta
    val beta_mx = Linear(.0, -beta, beta, .0)
    // matrix of rotation by tau
    val rot1_mx = Rotation(tau)
    val a_r = if (axisRatio >= 1.0) axisRatio else 1.0 / axisRatio
    // matrix of non-uniform scaling realizing axisRatio
    val axis_scale_mx = Scale(sqrt(a_r), 1.0/sqrt(a_r))
    // matrix of rotation by -tau
    val rot2_mx = Rotation(tau.complement)
    // matrix of uniform scaling realizing alfa
    val alfa_mx = UniScale(alfa)
    alfa_mx + (rot2_mx * axis_scale_mx * rot1_mx * beta_mx)
  }
}
