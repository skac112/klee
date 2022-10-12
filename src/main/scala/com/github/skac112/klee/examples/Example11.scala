package com.github.skac112.klee.examples

import cats.Id
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.klee.{Composition, drawToFile, trivialColorFun}
import com.github.skac112.vgutils.{Angle, Color, Point}
import com.github.skac112.vgutils._
import scala.math._
import com.github.skac112.klee.transforms.areas.Ring
import com.github.skac112.klee.transforms.gradients.Radial
import scala.math._

class Example11 {
  val rand = new scala.util.Random(1)
  val init_img = Fill[Color, Id](Color.hsla(Angle(235.toRadians), .8, .3))

  val funs = for {
    i <- 0 until 1500
    // a = 400.0
    a = max(.02, abs(450 + 250 * rand.nextGaussian()))
    val omega_f = 5
    val r = max(.1, 20.0/a)   
    l_ch_r = rand.nextGaussian()
    h_ch_r = rand.nextGaussian()
    s_ch_r = rand.nextGaussian()

    color_fun = (d: Double, color: Color) => {       
        val k = exp(-a*d)*cos(omega_f*a*d)
        val l_ch = max(-.3, (min(.7, k/8.0*(1 + l_ch_r))))
        val h_ch = .5*k*h_ch_r        
        val s_ch = k/10.0*s_ch_r            
        color.addH(Angle(h_ch)).addL(l_ch).addS(s_ch)      
    }

    radial = Radial[Color, Color, Id](Point(rand.nextDouble, rand.nextDouble), r, color_fun) 
  } yield radial

//   val bh = BlackHole[Color, Id](Point(.5, .5), 6 * Pi, 10.0, 1, 0, 1)
  val fun = Composition[Color, Id](funs.toList)
//   val fun = Composition[Color, Id](funs.toList ::: bh :: Nil)
  drawToFile[Color, Id](fun(init_img), trivialColorFun, "example11_49.png", 0, 1, 0, 1, 1000, 1000)
}
