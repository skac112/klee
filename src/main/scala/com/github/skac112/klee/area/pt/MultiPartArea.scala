package com.github.skac112.klee.area.pt

import com.github.skac112.klee.Points
import com.github.skac112.klee.area.img.ImgArea

case class MultiPartArea(parts: scala.collection.Seq[PtArea]) extends PtArea {
  lazy val points: Points = parts map { part => part.points } reduce { _ ++ _ }

  override def area: ImgArea = {
    val area_parts = parts map {part => part.area}
    com.github.skac112.klee.area.img.MultiPartArea(area_parts.toSet)
  }

  override def partByIntersect[T](imgArea: ImgArea): (PtArea, PtArea, PtArea, (scala.collection.Seq[T], scala.collection.Seq[T], scala.collection.Seq[T]) => scala.collection.Seq[T]) = {
    val parts_inters = parts map  { _.partByIntersect[T](imgArea) }
    val inside_pts = MultiPartArea(parts_inters map { _._1})
    val outside_pts = MultiPartArea(parts_inters map { _._2})
    val unknown_pts = MultiPartArea(parts_inters map { _._3})

    val borders = parts_inters.scanLeft((0, 0, 0)) { case (acc, part_inter) => { (acc._1 + part_inter._1.points.size,
      acc._2 + part_inter._2.points.size, acc._3 + part_inter._3.points.size) }}

    val join_fun = (inside: scala.collection.Seq[T], outside: scala.collection.Seq[T], unknown: scala.collection.Seq[T]) => {
        ((parts_inters zip borders) map { case ((inside_part, outside_part, unknown_part, part_join_fun), (inside_border, outside_border, unknown_border)) => {
          part_join_fun(
            inside.slice(inside_border, inside_border + inside_part.points.size),
            outside.slice(outside_border, outside_border + outside_part.points.size),
            unknown.slice(unknown_border, unknown_border + unknown_part.points.size))
        }}).reduce {_ ++ _}}

    (inside_pts, outside_pts, unknown_pts, join_fun)
  }
}
