package com.github.skac112.klee.area.pt

import com.github.skac112.klee.Points
import com.github.skac112.klee.area.img.ImgArea

case class MultiPartArea(parts: scala.collection.Seq[PtArea]) extends PtArea {
  lazy val points: Points = parts map { part => part.points } reduce { _ ++ _ }

  override def area: ImgArea = {
    val area_parts = parts map {part => part.area}
    com.github.skac112.klee.area.img.MultiPartArea(area_parts.toSet)
  }
}
