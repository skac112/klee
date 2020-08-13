package com.github.skac112.klee.transforms.areas

import com.github.skac112.klee.{Img, ImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Ring(c: Point, rl: Double, rh: Double, color: Color) extends ImgTrans {
  lazy val rl2 = rl*rl
  lazy val rh2 = rh*rh

  def apply(img: Img) = (p: Point) => {
    val mod2 = (p - c).modulus2
    if (mod2 >= rl2 && mod2 <= rh2) {
      color
    }
    else {
      img(p)
    }
  }
}
