package com.github.skac112.klee.area.pt

import com.github.skac112.klee.Points
import com.github.skac112.klee.area.img.ImgArea

case class MultiPartArea(parts: Seq[PtArea]) extends PtArea {
  override def points: Points = parts map { part => part.points } reduce { _ ++ _ }

  override def area: ImgArea = {
    val area_parts = parts map {part => part.area}
    com.github.skac112.klee.area.img.MultiPartArea(area_parts.toSet)
  }
}