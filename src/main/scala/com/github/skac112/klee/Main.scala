package com.github.skac112.klee

import cats.Id
import com.github.skac112.klee.examples._
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.linalg2d.spiral
import com.github.skac112.klee.transforms.areas.Circle
import com.github.skac112.vgutils.{Color, Point}
import com.github.skac112.klee.painters.Painter1
import com.github.skac112.klee.painters.Painter

object Main extends App {
     new Example16
//    val pp = Painter1.Painter1Params(0, 0, 0, 0, 0, 0, 0, 0, 0)
//    val rp = Painter.RenderParams(0, 1, 0, 1, 1000, 1000)
//    val p = new Painter1(pp, rp)
//    p.paint()

//   var end = false
//
//   val t1 = new Thread(new Runnable {
//     def run: Unit = {
//       scala.io.StdIn.readChar()
//       println("koniec")
//       Main.synchronized(
//         Main.end = true
//       )
//     }
//   })
//
//   t1.start
//   var i = 0
//   do {
//     println(i)
//     Thread.sleep(1000)
//     i = i + 1
//   } while (i < 10 && synchronized(!end))

//  val src_img = Fill[Color, Id](Color.white)
//  val circle = Circle[Color, Color, Id](Point(.5, .5), .25, Color.black)
//  val dst_img = circle(src_img)
//  val n = 100
//  drawToFile[Color, Id](dst_img, trivialColorFun, s"CircleTest.png", 0.0, 1.0, 0.0, 1.0, n, n)

//  new PolyMapExample
//  new Example2
//  val alfa = -.3
//  val beta = 1.5
//  val tau = .25*math.Pi
//  val axis_ratio = 6.0
//  val m = spiral(alfa, beta, tau, axis_ratio)
//  println(m.a)
//  println(m.b)
//  println(m.c)
//  println(m.d)
}
