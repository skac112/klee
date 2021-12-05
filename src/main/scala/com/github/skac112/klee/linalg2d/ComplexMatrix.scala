package com.github.skac112.klee.linalg2d

import breeze.math._
import com.github.skac112.vgutils.transform.linear._

case class ComplexMatrix(a: Complex, b: Complex, c: Complex, d: Complex) extends (ComplexVector => ComplexVector) {
  def *(factor: Double) = ComplexMatrix(a * factor, b * factor, c * factor, d * factor)
  def *(factor: Complex) = ComplexMatrix(a * factor, b * factor, c * factor, d * factor)
  def /(factor: Double) = ComplexMatrix(a / factor, b / factor, c / factor, d / factor)
  def /(factor: Complex) = ComplexMatrix(a / factor, b / factor, c / factor, d / factor)

  /**
    * Matrix multiplication of the form:
    * this * other.
    * @param other
    * @return
    */
  def *(other: ComplexMatrix) = ComplexMatrix(
    a * other.a + b * other.c,
    a * other.b + b * other.d,
    c * other.a + d * other.c,
    c * other.b + d * other.d)

  /**
    * Determinant
    */
  lazy val det = a * d - b * c

  /**
    * Inverse transform (matrix inverse).
    */
  lazy val inv = ComplexMatrix(d, -b, -c, a) / det

  lazy val real = Linear(a.re, b.re, c.re, d.re)

  def apply(v: ComplexVector) = ComplexVector(a*v.x + b*v.y, c*v.x + d*v.y)
}
