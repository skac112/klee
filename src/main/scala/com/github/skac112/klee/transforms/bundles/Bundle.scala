package com.github.skac112.klee.transforms.bundles

import cats.Monad
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.area.imgpt.ImgPtArea
import com.github.skac112.klee.images.raster.MutableRaster
import com.github.skac112.klee.{Img, ImgPoint, ImgPoints, LocalImgTrans, PureImgPoints}
import com.github.skac112.klee.transforms.PixelImg
import com.github.skac112.vgutils.{*, given}

object Bundle:
  type BlendFun[M[_]] = (ColorVector, Double) => M[ColorVector]

import Bundle._

trait Bundle[M[_]: Monad](rasterXResolution: Int, 
                          rasterYResolution: Int,
                          // It determines the ratio of raster pixel size / band sample pixel size 
                          bandSampleFactor: Double
                         ) extends LocalImgTrans[M]:
  def bundle(pt: Point)(using m: Monad[M]): (M[Point], BlendFun[M])

  override def apply(img: Img[M])(using m: Monad[M]): Img[M] = new Img[M] {
    val raster_img = createRasterImg()
    override def apply(p: Point)(implicit m: Monad[M]): M[ColorVector] = if (area.contains(p)) {
      raster_img(p)
    } else {
      img(p)
    }
    
    def createRasterImg(): MutableRaster[M] = ???
  }

  override def area: ImgArea = ???

//  override def applyInArea(img: Img[M], ip: ImgPoint[ColorVector, M])(using m: Monad[M]): ImgPoint[ColorVector, M] = ???

//  override def applyBatchInArea(img: Img[M], imgPoints: ImgPoints[ColorVector, M])(using m: Monad[M]): M[PureImgPoints[ColorVector]] = ???
  //  lazy val bitMap: Seq[Seq] = createBitmap

//  def createBitmap: Seq[Seq] =


//  override def pixelValue(x: Int, y: Int): M = ???