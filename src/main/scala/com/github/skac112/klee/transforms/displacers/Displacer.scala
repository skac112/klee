package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee._
import com.github.skac112.klee.area.img.{ImgArea, WholeArea}
import com.github.skac112.klee.area.pt.QuickPtArea
import com.github.skac112.klee.dynsys.vectormaps.VectorMap
import com.github.skac112.vgutils.{Color, Point}

object Displacer {
  type DispColorChangeFun = (Point, Point, Img) => Color
}

/**
  * Abstract base class for displacers. Value of image function for a point is taken from another point.
  * This other point is calculated as a displacement from given point. So, displacement value (vector) for a given point
  * determines value of a function (color) for given point and image.
  * CAUTION: Displacement vector is not equal to vector which transforms location of every point to a new
  * location along with its color. In such a model (which can be for example understood as an application of dynamical
  * system to image for some time shift) the displacement vector is an opposite of this first vector taken
  * for given end point. For example: if a displacer transform for given point p1 translates it to point p2 = p1 +
  * transl, the displacement vector for point p2 (not p1) is equal to -transl. So, the displacement is a "lookup" vector
  * which is used to take a value from (combined with location of base point).
  */
trait Displacer extends LocalImgTrans {
  def displacement: VectorMap

  /**
    * Default area is the whole area.
    * @return
    */
  def area: ImgArea = WholeArea()

  def applyInArea(img: Img, p: Point): Color = {
    img(p + displacement(p))
  }

  override def applyBatchInArea(img: Img, points: Points): Colors = {
    val disp_points = displacement.applyBatchArea(QuickPtArea(points, area))
    img.applyBatch((points zip disp_points) map {pt_pair: (Point, Point) => pt_pair._1 + pt_pair._2})
  }
}
