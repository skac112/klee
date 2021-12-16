package com.github.skac112.klee.transforms.areas

import com.github.skac112.klee.{Img, ImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Ring[T](c: Point, rl: Double, rh: Double, color: T) extends ImgTrans[T] {
  lazy val rl2 = rl*rl
  lazy val rh2 = rh*rh

  def apply(img: Img[T]) = (p: Point) => {
    val mod2 = (p - c).modulus2
    if (mod2 >= rl2 && mod2 <= rh2) {
      color
    }
    else {
      img(p)
    }
  }
}
