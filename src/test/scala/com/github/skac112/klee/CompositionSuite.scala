package com.github.skac112.klee

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.vgutils.{Color, Point}

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

class CompositionSuite extends AnyFlatSpec with should.Matchers {
  "A Composition" should "be an actual composition, goddamn!" in {
    val src_img = Fill[Color, Id](Color.white)
    val red_circle = Circle[Color, Color, Id](Point(.4, .5), .25, Color.red(.7))
    val blue_circle = Circle[Color, Color, Id](Point(.6, .5), .25, Color.blue(.7))
    val comp = Composition(Seq(red_circle, blue_circle))
    val dst_img = comp(src_img)
    val n = 100
    drawToFile[Color, Id](dst_img, trivialColorFun, s"CompositionTest.png", 0.0, 1.0, 0.0, 1.0, n, n)
  }
}
