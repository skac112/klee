package com.github.skac112.klee.area.pt

import com.github.skac112.vgutils.{Bounds, Point}

import collection.mutable.Stack
import org.scalatest._
import flatspec._
import matchers._

class AxisGridSuite extends AnyFlatSpec with should.Matchers {
  def fixture = new {
    val grid = new AxisGrid(Point(0, 0), 100, 100, 1.0, 1.0)
  }

  "AxisGrid with top left point (0, 0), 100 columns and 100 rows width dx = dy = 1" should "be properly partitioned by image area with bounds" in {
    val f = fixture
    val img_area = new com.github.skac112.klee.area.img.BoundsArea(new Bounds(Point(25, 25), Point(75, 75)))
    val partition = f.grid.partByIntersect(img_area)
    val inside_area = partition._1
    val outside_area = partition._2
    val unknown_area = partition._3
    println(unknown_area.bounds)
    assert(inside_area.points.size == 0, "Inside area should be empty")
    assert(unknown_area.points.size == 2500)
  }
}
