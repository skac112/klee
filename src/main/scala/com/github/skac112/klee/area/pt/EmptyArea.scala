package com.github.skac112.klee.area.pt

import com.github.skac112.klee.area._

case class EmptyArea() extends PtArea {
  override lazy val points = Seq()
  override lazy val area = img.EmptyArea()
}
