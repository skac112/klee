package com.github.skac112.klee

import cats.Monad
import com.github.skac112.klee.flows.vectormaps.{PolyMap, VectorMap}
import com.github.skac112.vgutils._
import cats.implicits._

import scala.annotation.tailrec
import scala.math._

package object flows {
  val f1_6 = 1.0 / 6.0

  /**
    * Dynamical system representing points evolution in time:
    * x -> f(x, t)
    */
//  type DynamicalSystem = (Point, Double) => Point

  /**
    * Fourth order Runge-Kutta method for points (function fun is a time-derivative of a point p):
    * p' = fun(p) = dp/dt(p).
    * @param fun
    * @param p
    * @param h
    * @return
    */
  def rungeKutta4[M[_]: Monad](fun: VectorMap[M], p: Point, h: Double) = for {
    k1 <- fun(p)
    k2 <- fun(p + (k1 * h * .5))
    k3 <- fun(p + (k2 * h * .5))
    k4 <- fun(p + (k3 * h * .5))
  } yield p + (k1 + (k2 * 2) + (k3 * 2) + k4) * f1_6

  private def binomialCoeff(n: Int, i: Int): Int = factorial(n) / factorial(i) / factorial(n - i)

  private def factorial(n: Int): Int = n match {
    case 0 => 1
    case 1 => 1
    case n => n * factorial(n - 1)
  }

  /**
    * Calculates taylor expansion for given function around base point up to a given order using
    * step parameter h.
    * @param fun
    * @param base
    * @param order
    * @param h
    * @return
    */
  def taylorExp[M[_]](fun: Point => Point, base: Point, taylorOrder: Int, h: Double)(implicit m: Monad[M]): PolyMap[M] = new PolyMap[M] {
//    override val m = implicitly[Monad[M]]
    override val initCoeffs = calcTaylorCoeffs
    override def apply(p: Point)(implicit m: Monad[M]) = super.apply(p - base)
    private lazy val funVals: Seq[Seq[Point]] = (0 to taylorOrder) map {i => { (0 to taylorOrder - i) map {j => fun(base + Point(i * h, j * h))}}}

    /**
      * Calculates (-1)^n for given n
      * @param power
      */
    private def alterSign(power: Int) = if (power % 2 == 0) 1 else -1

    private def calcTaylorCoeffs: Seq[Seq[Point]] = {
      val x_derivs: Seq[IndexedSeq[Point]] = (0 to taylorOrder) map { nx =>
        // nx is an index of column in two-dimensional funVals array and also an order of x-derivative
        // calculated
        val h_pow = math.pow(h, nx)
        // for given x column only order - nx x-derivatives are calculated (plus the function value itself
        // as a 0-derivative)
        (0 to taylorOrder - nx) map { ny =>
          ((0 to nx) map { i => funVals(nx - i)(ny) * alterSign(i) * binomialCoeff(nx, i) } reduce {_ + _}) / h_pow
        }
      }
      // xy_derivs are calculated analogously with direction of most inner loop flipped and on x_derivs instead of funVals
      val xy_derivs: Seq[Seq[Point]] = (0 to taylorOrder) map { nx =>
        (0 to taylorOrder - nx) map { ny =>
          val h_pow = math.pow(h, ny)
          ((0 to ny) map { i => x_derivs(nx)(ny - i) * alterSign(i) * binomialCoeff(ny, i)} reduce {_ + _}) / h_pow
        }}
      // result coeffs are xy_derivs divided by appropriate factorials
      xy_derivs.zipWithIndex map { case (col, col_idx) =>
        val col_w_idx = col.zipWithIndex
        col_w_idx map { case (deriv, row_idx) => deriv / factorial(row_idx) / factorial(col_idx)}}
    }
  }

  implicit def pointFunToVectorMap[M[_]: Monad](fun: ((Point) => Point)): VectorMap[M] = new VectorMap[M] {
//    override val m = implicitly(Monad[M])
    override def apply(p: Point)(implicit m: Monad[M]) = m.pure(fun(p))
  }
}
