package com.github.skac112.klee.transforms.bundles

import cats.Monad
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgPoint, ImgPoints, LocalImgTrans, PureImgPoints}
import com.github.skac112.klee.transforms.PixelImg
import com.github.skac112.vgutils.Point

object Bundle:
//  type BlendFun[I, M[_]] = (I, I) => M[I]
  type BlendFun[I, M[_]] = (I, Double, I, Double) => M[I]
//  type BundleFun[I, M[_]] = Point => (M[Point], I, BlendFun[I, M])

import Bundle._

trait Bundle[I, M[_]: Monad](width: Int, height: Int) extends LocalImgTrans[I, M]:
  def bundle(pt: Point)(using m: Monad[M]): (M[Point], I, Double, BlendFun[I, M])

  override def apply(img: Img[I, M])(using m: Monad[M]): Img[I, M] = ???
  
  override def area(using m: Monad[M]): ImgArea = ???

  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M])(using m: Monad[M]): ImgPoint[I, M] = ???

  override def applyBatchInArea(img: Img[I, M], imgPoints: ImgPoints[I, M])(using m: Monad[M]): M[PureImgPoints[I]] = ???
  //  lazy val bitMap: Seq[Seq[I]] = createBitmap

//  def createBitmap: Seq[Seq[I]] =


//  override def pixelValue(x: Int, y: Int): M[I] = ???

