package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee._
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.area.pt.QuickPtArea
import com.github.skac112.klee.flows.vectormaps.VectorMap
import com.github.skac112.vgutils.{Color, Point}
import cats.Monad
import cats.implicits._

object Displacer {
  type DispColorChangeFun[T, M[_]] = (Point, Point, Img[T, M]) => T
}

/**
  * Abstract base class for displacers. Value of image function for a point is taken from another point.
  * This other point is calculated as a displacement from given point. So, displacement value (vector) for a given point
  * determines value of a function (color) for given point and image.
  * CAUTION: Displacement vector is not equal to vector which transforms location of every point to a new
  * location along with its color. Value of displacement is the opposite of transformation of location of points. In such a model 
  * (which can be for example understood as an application of dynamical
  * system to image for some time shift) the displacement vector is an opposite of this first vector taken
  * for given end point. For example: if a displacer transform for given point p1 translates it to point p2 = p1 +
  * transl, the displacement vector for point p2 (not p1) is equal to -transl. So, the displacement is a "lookup" vector
  * which is used to take a value from (combined with location of base point). 
  */
abstract class Displacer[I, M[_]: Monad] extends LocalImgTrans[I, I, M] {
  def displacement: VectorMap[M]
//  override implicit val ev: I <:< I = implicitly(ev: I <:< I)
//  override implicit val evSeq: Seq[I] <:< Seq[I] = implicitly(evSeq: Seq[I] <:< Seq[I])

  /**
    * Default area is the whole area.
    * @return
    */
  def area: ImgArea = WholeArea()

  override def applyInArea(img: Img[I, M], p: Point): M[I] = for {
    disp <- displacement(p)
    out <- img(p + disp)
  } yield out

  override def applyBatchInArea(img: Img[I, M], points: Points): M[scala.collection.Seq[I]] = implicitly[Monad[M]].flatMap(
    displacement.applyBatchArea(QuickPtArea(points, area)))(disp_points => img.applyBatch((points zip disp_points) map {pt_pair: (Point, Point) => pt_pair._1 + pt_pair._2}))

//  override def applyBatchInArea(img: Img[I, M], points: Points): M[Seq[I]] = for {
//    disp_points: Seq[Point] <- displacement.applyBatchArea(QuickPtArea(points, area))
//    seq <- img.applyBatch((points zip disp_points) map {pt_pair: (Point, Point) => pt_pair._1 + pt_pair._2})
//  } yield seq
}
