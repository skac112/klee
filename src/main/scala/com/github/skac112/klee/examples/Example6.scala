package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Ring
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFileOld, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import scala.math._
import com.github.skac112.klee.images.Lines
import com.github.skac112.klee.ImgTrans

class Example6 {
  val c = Point(.5, .5)
  val init_img = Lines[Color, Id](0, 0, .03, 0.03, .005, Color.black, Color.white)
//   val init_img = Fill[Color, Id](Color.white)
  val bh = BlackHole[Color, Id](c, 5 * Pi, 10, 1.0, 0, .7)

//   val rings = (0 until 6) map {i: Int =>
//     Ring[Color, Color, Id](c + Point(0.05*i, .05*i), .2, .25, Color.red(.1 * i))
//   }

  val rings = (0 until 5) map {i: Int =>
    Ring[Color, Id](c, .1 + .08*i, .13 + .09*i, Color(.2, .4 + .05*i, .6 - .02*i))
  }

  
  val colorDispFun = (c: Color, p: Point) => c.addH(p.modulus*5 + p.angle*.1).toColor()
  val count = 3
  val r = .4

  val bhs = (0 until count).map { i =>
    val angle = Angle(2 * Pi * i / count)
    val rot = i match {
        case 0 => 3 * Pi
        case 1 => 5 * Pi
        case 2 => 7 * Pi
    }    
    com.github.skac112.klee.transforms.displizers.BlackHole[Color, Id](c + new Point(r * (1.0 + i/10), angle), 3 * Pi, 10, colorDispFun, .7)
  }

  val comp = Composition(rings ++ bhs)
  drawToFileOld[Color, Id](init_img, trivialColorFun, "example7_1.png", 0, 1, 0, 1, 1500, 1500)
//   drawToFile[Color, Id](comp(init_img), trivialColorFun, "example7_1.png", 0, 1, 0, 1, 1000, 1000)
}
