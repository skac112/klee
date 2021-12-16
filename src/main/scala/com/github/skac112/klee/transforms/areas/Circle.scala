package com.github.skac112.klee.transforms.areas

import com.github.skac112.klee.{Img, ImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Circle[T](c: Point, r: Double, color: T) extends ImgTrans[T] {

  lazy val r2 = r*r

  def apply(img: Img[T]) = (p: Point) => {
    if ((p - c).modulus2 <= r2) {
      color
    }
    else {
      img(p)
    }
  }
}
