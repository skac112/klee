package com.github.skac112.klee.flows.vectormaps

import cats.Monad
import com.github.skac112.vgutils.Point
import scala.language.postfixOps

/**
  * VectorMap in the form of a polynomial of two variables (x and y coordinates of a point).
  **/
abstract class PolyMap[M[_]: Monad] extends VectorMap[M] {
//  override val m = implicitly[Monad[M]]
  /**
    * Coefficient for n-power of x or y has for each of output value (x or y) has index equal to n (1-st or second,
    * appropriatelly), i.e. outer indices span x powers increasingly, inner indices span y powers increasingly and each
    * of two polynomials (for x and y output value) is a sum of
    * terms:
    * coeffs[k][l]*x^k*y^l,
    * for k = 0, 1, 2, ..., order and l(k) = 0, 1, ..., order - k where ^ means power
    * Submitted values don't need to have any particular shape. Desirable triangular structure is obtained
    * bases on this values as a coeffs val.
    */
  def initCoeffs: Seq[Seq[Point]]

  /**
    * Order is by default calculated as a highest sum of non-zero powers (and indices) of coeffs from every
    * pair of indices. Overriding default implementation enables to force higher order than actual (in a polynomial sense).
    * This can be helpful in some calculations to avoid over-truncation of order.
    * @return
    */
  def order =
    (initCoeffs zipWithIndex) map { case (y_coeffs, idx) =>
      idx + y_coeffs.lastIndexWhere(_ != Point(0.0, 0.0)) } max

  /**
    * Coefficients of polynomial with a triangular structure (flipped upper triangle matrix of size order + 1).
    */
  lazy val coeffs: Seq[Seq[Point]] = 0 to order map { i => (0 to order - i) map { initCoeffs(i)(_)}}

//  def size = coeffs.size

  /**
    * Calculates value of a polynomial using 2-dimensional horner scheme.
    * @param p
    * @return
    */
  override def apply(p: Point)(implicit m: Monad[M]) = {
    val y_horners = initCoeffs map { (y_coeffs: Seq[Point]) => horner1dim(p.y, y_coeffs) }
    m.pure(horner1dim(p.x, y_horners))
  }

  /**
    * Calculates values of two polynomials (for out x and y of a point) of one variable for given x using horner scheme.
    * @param x
    * @param coeffs
    */
  private def horner1dim(x: Double, coeffs: Seq[Point]): Point = coeffs.reduceRight((left, right) => left + right*x)
}
