package com.github.skac112.klee.transcomb

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.{Img, ImgPoint, InstantImgPoint, LandImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.Point

/**
  * Local image transform with additional "mixing" step - for points in image trans area a resulting color is calculated
  * as a result of supplied mixing function (mixingFun). This function takes two values as inputs : color (value of
  * type I) of input image in given point and color obtained from applying supplied local image trans (baseTrans) to
  * the given point.
  * @param mixingFun
  * @param monad$M$0
  * @tparam I
  * @tparam M
  */
case class LocalMixer[M[_]](
                                       baseTrans: LocalImgTrans[M],
                                       mixingFun: (I, I) => M) extends LocalImgTrans[M] {

  override def area(implicit m: Monad[M]) = baseTrans.area

  override def applyInArea(img: Img[M], ip: ImgPoint[M])(implicit m: Monad[M]): ImgPoint[M] = if (ip.land) {
      val color_m = for {
        trans_color <- baseTrans.applyInArea(img, ip).color
        img_color <- img.applyM(ip.point)
        color <- mixingFun(trans_color, img_color)
      } yield color
      InstantImgPoint(ip.point, color_m)
    } else {
      // Identity function for air points (due to ip parameter)
      ip
    }
}
