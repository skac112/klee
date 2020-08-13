package com.github.skac112.klee.transforms.displacers

import com.github.skac112.klee.Img
import com.github.skac112.klee.dynsys.DynamicalSystem
import com.github.skac112.klee.transforms.displacers.Displacer._
import com.github.skac112.vgutils.{Color, Point}

case class DynSysDisplacer(dynSys: DynamicalSystem, time: Double, colorChangeFunO: Option[DispColorChangeFun] = None) extends Displacer {
  override def colorChangeFun(srcPt: Point, displacement: Point, img: Img): Color = colorChangeFunO match {
    case Some(fun) => fun(srcPt, displacement, img)
    case None => img(srcPt + displacement)
  }

  override def displacement(p: Point, img: Img) = dynSys(p, -time)
}