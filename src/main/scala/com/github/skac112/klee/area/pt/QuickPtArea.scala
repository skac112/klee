package com.github.skac112.klee.area.pt

import com.github.skac112.klee.Points
import com.github.skac112.klee.area.img.ImgArea

case class QuickPtArea(override val points: Points, override val area: ImgArea) extends PtArea
