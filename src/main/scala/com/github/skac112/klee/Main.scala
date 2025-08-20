package com.github.skac112.klee

import cats.Id
import com.github.skac112.klee.area.img.{MultiPartArea, Rect}
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.linalg2d.spiral
import com.github.skac112.klee.painters.FingerComb.FingerCombParams
import com.github.skac112.klee.painters.FingerSun.FingerSunParams
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.vgutils.{Angle, Bounds, Color, Point}
import com.github.skac112.klee.painters.*
import com.github.skac112.klee.painters.Painter.RenderParams
import com.github.skac112.klee.painters.Painter1.Painter1Params
import com.github.skac112.klee.paintrunners.*
import com.github.skac112.klee.serialize.given
import upickle.default._
import com.github.skac112.vgutils.ColorVector

import scala.collection.immutable.HashSet

object Main extends App {
//    val params = FingerSunParams(numFinger = 30)
//
//    val render_params = RenderParams(
//        Bounds(Point(-.5, -.5), Point(.5, .5)),
//        nx = 1000,
//        ny = 1000)
//
//    val painter = new FingerSun(params, render_params)
//    painter.paint()

    // Przykład serializacji i deserializacji BlackHole
    val bh: ImgTrans.Simple[ColorVector, Id] = com.github.skac112.klee.transforms.displacers.BlackHole[ColorVector, Id](
      com.github.skac112.vgutils.Point(0.1, 0.2),
      rotation = 1.5,
      rotationDecay = 0.1,
      scaling = 2.0,
      scalingDecay = 0.05,
      areaRadius = 0.3
    )

    val bhJson = write(bh)
    println(s"BlackHole jako JSON: $bhJson")

    val bhDeserialized = read[ImgTrans.Simple[ColorVector, Id]](bhJson)
    println(s"Deserializowany BlackHole: $bhDeserialized")
}
