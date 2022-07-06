package com.github.skac112.klee.transforms.displacers

import cats.Monad
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.klee.transforms.displacers.Displacer.DispColorChangeFun
import com.github.skac112.klee.{Img, ImgTrans}
import com.github.skac112.vgutils.{Angle, Color, Point}

case class BlackHole[I, M[_]: Monad](c: Point,
                     rotation: Double,
                     rotationDecay: Double,
                     scaling: Double,
                     scalingDecay: Double) extends Displacer[I, M] {
  override def displacement = new VectorMap[M] {
    override val m = implicitly(Monad[M])

    override def apply(p: Point) = m.pure(rotDisplacement(p))
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
