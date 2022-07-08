package com.github.skac112.klee.transforms.areas

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.{drawToFile, trivialColorFun}
import com.github.skac112.vgutils.{Color, Point}

import collection.mutable.Stack
import org.scalatest._
import org.scalatest._
import flatspec._
import matchers._

class CircleTest extends AnyFlatSpec with should.Matchers {

  "A Circle" should "be an actual circle, goddamn!" in {
    val src_img = Fill[Color, Id](Color.white)
    val circle = Circle[Color, Color, Id](Point(.5, .5), .25, Color.black)
    val dst_img = circle(src_img)
    val n = 100
    drawToFile[Color, Id](dst_img, trivialColorFun, s"CircleTest.png", 0.0, 1.0, 0.0, 1.0, n, n)
  }
}
