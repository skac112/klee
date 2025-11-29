package com.github.skac112.klee.transforms.displizers

import cats.Monad
import com.github.skac112.klee.{Img, ImgPoint}
import com.github.skac112.vgutils.{ColorVector, Point}
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.img.WholeArea

import scala.collection.immutable.VectorMap

final case class BlackHole[M[_]](
  c: Point,
  rotation: Double,
  rotationDecay: Double,
  override val colorDispFun: (ColorVector, Point) => M[ColorVector],
  areaRadius: Double = .0) extends Displizer[M] {
    def dispBh = com.github.skac112.klee.transforms.displacers.BlackHole[M](c, rotation, rotationDecay, 1.0, 0.0, areaRadius)
    override def area = dispBh.area
    override def displacement(implicit m: Monad[M]) = dispBh.displacement

    override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = ???
}
