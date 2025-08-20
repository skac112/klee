package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.area.img.{Circle, WholeArea}
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.transforms.displacers.Displacer.DispColorChangeFun
import com.github.skac112.klee.{Img, ImgTrans}
import com.github.skac112.vgutils.{*, given}

case class BlackHole[M[_]](
  c: Point,
  rotation: Double,
  rotationDecay: Double,
  scaling: Double,
  scalingDecay: Double,
  areaRadius: Double = 0.0) extends Displacer[ColorVector, M] {

  override def area(implicit m: Monad[M]) = if (areaRadius != 0) Circle(c, areaRadius) else WholeArea()

  override def displacement(implicit m: Monad[M]) = new VectorMap[M] {
    override def apply(p: Point)(implicit m: Monad[M]) = m.pure(rotDisplacement(p))
  }

  /**
    * Rotation displacement vector.
    */
  private def rotDisplacement(p: Point) = cVec(p).rot(-pRot(p)) - cVec(p)

  def cDist(p: Point) = cVec(p).modulus
  def cVec(p: Point) = p - c
  def pRot(p: Point): Double = rotation * math.exp(-cDist(p)*rotationDecay)

  /**
    * Scaling displacement vector
    */
  private def scalingDisplacement(p: Point) = {
    val dist = cDist(p)
//    p - c = (p2 - c) - (p2 - c)*(1 - scaling)*math.exp(-dist*scalingDecay)
//    p - c = (p2 - c)*(1 - (1 - scaling)*math.exp(-dist*scalingDecay))
//    p2 - c = (p - c) / (1 - (1 - scaling)*math.exp(-dist*scalingDecay))
    val disp_len = dist / (1 - (1 - scaling)*math.exp(-dist*scalingDecay)) - dist
    Point(disp_len, cVec(p).angle - pRot(p))
  }
}
