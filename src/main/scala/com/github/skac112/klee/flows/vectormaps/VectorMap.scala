package com.github.skac112.klee.flows.vectormaps

import cats.Monad
import com.github.skac112.klee.{Img, ImgPoint, ImgPoints, InstantImgPoint, InstantPureImgPoint, Points, PureImgPoints, RealColors, Values}
import com.github.skac112.vgutils.{ColorVector, Point}
import com.github.skac112.vgutils.transform.linear.*
import cats.implicits.*
import com.github.skac112.klee.area.imgpt.ImgPtArea
import com.github.skac112.klee.flows.*

object VectorMap {
  def from[M[_]: Monad](fun: Point => Point) = new VectorMap[M] {
//    override val m = implicitly[Monad[M]]
    override def apply(p: Point)(implicit m: Monad[M]) = m.pure(fun(p))
  }

  def identity[M[_]]: VectorMap[M] = new VectorMap[M] {
//    override val m = implicitly[Monad[M]]
    override def apply(p: Point)(implicit m: Monad[M]): M[Point] = m.pure(p)
  }
}

abstract class VectorMap[M[_]] {
  self =>
//  override val m = implicitly[Monad[M]]
  lazy val invMap: VectorMap[M] = ???
  lazy val f1_6 = 1.0 / 6

  def apply(p: Point)(implicit m: Monad[M]): M[Point]

  /**
    * Transforms this vector map to a new vector map making one step of integration using given
    * vector map as a velocity field.
    *
    * @param h
    */
  def rungeKutta4(motionEq: VectorMap[M], h: Double)(implicit m: Monad[M]): VectorMap[M] = {
    // caution: all operations in the body of method are performed on vectormaps (functions) rather than on a
    // points or numbers (implicitly k1 = motionEq).
    val k2: VectorMap[M] = motionEq compose (VectorMap.identity[M] + (motionEq * .5 * h))
    val k3: VectorMap[M] = motionEq compose (VectorMap.identity[M] + (k2 * .5 * h))
    val k4: VectorMap[M] = motionEq compose (VectorMap.identity[M] + (k3 * h))
    this + (motionEq + (k2 * 2.0) + (k3 * 2.0) + k4) * h * f1_6
  }

  def *(factor: Double)(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {
//    override val m = self.m

    def apply(p: Point)(implicit m: Monad[M]): M[Point] = for {
      p2 <- self(p)
    } yield p2 * factor
  }

//  def *(factor: Double): VectorMap[M] = new VectorMap[M] {
//    override val m = self.m
//
//    override def apply(p: Point): M[Point] = m.map(self(p))(_ * factor)
//  }

  def /(factor: Double)(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {
//    override val m = VectorMap.this.m

    override def apply(p: Point)(implicit m: Monad[M]): M[Point] = m.map(self(p))(_ / factor)
  }

  def +(otherPt: Point)(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {
//    override val m = VectorMap.this.m

    override def apply(p: Point)(implicit m: Monad[M]): M[Point] = m.map(self(p))(_ + otherPt)
  }

  def -(otherPt: Point)(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {
//    override val m = VectorMap.this.m
    override def apply(p: Point)(implicit m: Monad[M]): M[Point] = m.map(self(p))(_ - otherPt)
  }

  def +(otherMap: VectorMap[M])(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {
//    override val m = VectorMap.this.m

    override def apply(p: Point)(implicit m: Monad[M]) = m.flatMap(self(p))(p2 => m.map(otherMap(p))(p3 => p2 + p3))
  }

  def -(otherMap: VectorMap[M])(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {
//    override val m = VectorMap.this.m

    override def apply(p: Point)(implicit m: Monad[M]) = m.flatMap(self(p))(p2 => m.map(otherMap(p))(p3 => p2 - p3))
  }

//  def -(otherMap: VectorMap[M]): VectorMap[M] = new VectorMap[M] {
//    override val m = VectorMap.this.m
//    override def apply(p: Point): M[Point] = for {
//      p1 <- outerApply(p)
//      p2 <- otherMap(p)
//    } yield p1 - p2
//  }

  def jacobi(p: Point): Linear = ???

  /**
    * This implementation is analogous to and hides base class (Function2) implementation. The obvious difference is
    * that functions' output is wrapped in a Monad M, so unwrapping is needed when composing. Other is applied first
    * (is inner).
    * @param other
    * @return
    */
  def compose(other: VectorMap[M])(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {
//    override val m = self.m
    override def apply(p: Point)(implicit m: Monad[M]) = m.flatMap(other(p))(self.apply)
  }

  /*
    * This implementation is analogous to and hides base class (Function2) implementation.
    * @param other
    * @return
    */
  def andThen(other: VectorMap[M])(implicit m: Monad[M]): VectorMap[M] = new VectorMap[M] {
//    override val m = self.m
    override def apply(p: Point)(implicit m: Monad[M]) = m.flatMap(self(p))(other.apply)
  }

  /**
    * Base implementation just evaluates each point independently, but custom implementations
    * can make performance improvements.
    *
    * @param points
    * @return
    */
  //  override def applyBatch(points: Points): Points = points map apply _

  /**
    * Base implementation just evaluates each point independently, but custom implementations
    * can make performance improvements.
    * @param points
    * @return
    */
  def jacobiBatch(points: Points): scala.collection.Seq[Linear] = points map jacobi _

  /**
    * Base implementation just evaluates each point independently.
    *
    * @param points
    * @return
    */
  def applyBatchArea(imgPts: ImgPoints[M])(using m: Monad[M]): Values[M[Point]] = imgPts map { (ip: ImgPoint[M]) =>
    if (ip.land) {
      applyM(ip.point)
    } else {
      // for air point, the inverted transformation is used
      invMap.applyM(ip.point)
    }
  }

  private def applyM(ptM: M[Point])(implicit m: Monad[M]): M[Point] = for {
    pt <- ptM
    value <- apply(pt)
  } yield value
}
