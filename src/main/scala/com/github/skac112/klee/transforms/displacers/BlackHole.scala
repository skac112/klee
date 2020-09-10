package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee.transforms.displacers.Displacer.DispColorChangeFun
import com.github.skac112.klee.{Img, ImgTrans}
import com.github.skac112.vgutils.{Angle, Color, Point}

case class BlackHole(c: Point,
                     rotation: Double,
                     rotationDecay: Double,
                     scaling: Double,
                     scalingDecay: Double) extends Displacer {
  override def displacement =
    // displacement is a composition of scale moving and rotation - all around point c
    rotDisplacement
//    rotDisplacement(p) + scalingDisplacement(p)

  /**
    * Rotation displacement vector.
    */
  private def rotDisplacement(p: Point) = cVec(p).rot(-pRot(p)) - cVec(p)

//  override def colorChangeFun(srcPt: Point, displacement: Point, img: Img): Color = colorChangeFunO match {
//    case Some(fun) => fun(srcPt, displacement, img)
//    case None => img(srcPt + displacement)
//  }

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
