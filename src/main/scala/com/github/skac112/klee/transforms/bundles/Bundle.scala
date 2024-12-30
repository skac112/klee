package com.github.skac112.klee.transforms.bundles

import cats.Monad
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgPoint, ImgPoints, LocalImgTrans, PureImgPoints}
import com.github.skac112.klee.transforms.PixelImg
import com.github.skac112.vgutils.Point

object Bundle:
//  type BlendFun[I, M[_]] = (I, I) => M[I]
  type BlendFun[I, M[_]] = (I, Double, I, Double) => M[I]
  type BundleFun[I, M[_]] = Point => (M[Point], I, BlendFun[I, M])

import Bundle._

trait Bundle[I, M[_]](width: Int, height: Int) extends LocalImgTrans[I, M]:
  def bundleFun: BundleFun[I, M]

  override def area(implicit m: Monad[M]): ImgArea = ???

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M])(implicit m: Monad[M]): ImgPoint[I, M] = ???

  override def applyBatchInArea(img: Img[I, M], imgPoints: ImgPoints[I, M])(implicit m: Monad[M]): M[PureImgPoints[I]] = ???
  //  lazy val bitMap: Seq[Seq[I]] = createBitmap

//  def createBitmap: Seq[Seq[I]] =


//  override def pixelValue(x: Int, y: Int): M[I] = ???

