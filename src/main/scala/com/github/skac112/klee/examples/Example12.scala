package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFile, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import com.github.skac112.vgutils._
import scala.math._
import com.github.skac112.klee.transforms.gradients.Ring
import com.github.skac112.klee.transforms.gradients.Radial

class Example12 {
  val rand = new scala.util.Random(11)
  val init_img = Fill[Color, Id](Color.hsla(Angle(Pi), .7, .5))

//   val color_fun = (d: Double, color: Color) => {
//     val a = 10
//     val b = .1
//     val k = 1.0/(d*a + b)*b
//     color.addL(k/3).addH(k/2)   
//   }

  val rings = for {
    i <- 0 until 300
    a = math.abs(30 + 15 * rand.nextGaussian())
    val r = 12/a
    b = math.abs(rand.nextGaussian() * .05)
    l_ch_r = rand.nextGaussian()
    h_ch_r = rand.nextGaussian()
    s_ch_r = rand.nextGaussian()
    val rr = max(0, 0.015 + .02 * rand.nextGaussian)

    color_fun = (d: Double, color: Color) => {
        // val k = b/(d*d*a*a + b)
        val k = b / (b + d*d*a*a + .3*d*d*d*a*a*a*(1 + math.sin(d*a*a)))
        val l_ch = k/8*l_ch_r
        val h_ch = k/.5*h_ch_r        
        val s_ch = k/6*s_ch_r            
        color.addH(Angle(h_ch)).addL(l_ch).addS(s_ch)      
    }

    ring = Ring[Color, Color, Id](Point(rand.nextDouble, rand.nextDouble), rr, 2*r, color_fun) 
  } yield ring

  val bh = BlackHole[Color, Id](Point(.5, .5), 4 * Pi, 10.0, 1, 0, 1)
//   val fun = Composition[Color, Id](rings.toList)
  val fun = Composition[Color, Id](rings.toList ::: bh :: Nil)
  drawToFile[Color, Id](fun(init_img), trivialColorFun, "example12_7.png", 0, 1, 0, 1, 1000, 1000)
}
