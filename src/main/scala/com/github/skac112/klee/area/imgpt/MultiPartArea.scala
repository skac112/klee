package com.github.skac112.klee.area.imgpt

import cats.Monad
import cats._
import cats.implicits._
import com.github.skac112.klee.{Points, PureImgPoints}
import com.github.skac112.klee.area.img.ImgArea

object MultiPartArea {
  type ImgPtAreas[I, M[_]] = scala.collection.Seq[ImgPtArea[I, M]]
}

import MultiPartArea._

case class MultiPartArea[I, M[_]: Monad](parts: ImgPtAreas[I, M]) extends ImgPtArea[I, M] {
  lazy val imgPoints = parts map { part => part.imgPoints } reduce { _ ++ _ }

  override def area: ImgArea = {
    val area_parts = parts map {part => part.area}
    com.github.skac112.klee.area.img.MultiPartArea(area_parts.toSet)
  }

  override def partByIntersect[O](imgArea: ImgArea): ThisPartFunRes[O]  = for {
    parts_inters <- (parts map { _.partByIntersect[O](imgArea) }).toVector.sequence
    val inside_pts = MultiPartArea(parts_inters map { _._1})
    val outside_pts = MultiPartArea(parts_inters map { _._2})
    val unknown_pts = MultiPartArea(parts_inters map { _._3})

    val borders = parts_inters.scanLeft((0, 0, 0)) { case (acc, part_inter) => { (acc._1 + part_inter._1.imgPoints.size,
      acc._2 + part_inter._2.imgPoints.size, acc._3 + part_inter._3.imgPoints.size) }}

    val join_fun = (inside: PureImgPoints[O], outside: PureImgPoints[O], unknown: PureImgPoints[O]) => {
        ((parts_inters zip borders) map { case ((inside_part, outside_part, unknown_part, part_join_fun), (inside_border, outside_border, unknown_border)) => {
          part_join_fun(
            inside.slice(inside_border, inside_border + inside_part.imgPoints.size),
            outside.slice(outside_border, outside_border + outside_part.imgPoints.size),
            unknown.slice(unknown_border, unknown_border + unknown_part.imgPoints.size))
        }}).reduce {_ ++ _}}
  } yield (inside_pts, outside_pts, unknown_pts, join_fun)
}
