package com.github.skac112.klee

import com.github.skac112.vgutils.transform.linear.Linear

import scala.math.sqrt

package object linalg2d {

  /**
    * Synthesizes 2d-matrix of a spiral node linear dynamical system. Eigenvalues  are complex conjugate and have form:
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
  def spiral(alfa: Double, beta: Double, tau: Double, axisRatio: Double): Linear = {
    // Mmodulus of each eigenvalue
    val eigenModul = sqrt(alfa*alfa + beta*beta)
    val ar4 = axisRatio*axisRatio*axisRatio*axisRatio
    // length of vector related to of smaller ellipse axis
    val v2n = sqrt(1.0/(ar4+1.0))
    // length of vector related to of larger ellipse axis
    val v1n = axisRatio*axisRatio*v1n

  }
}
