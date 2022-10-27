package com.github.skac112.klee.painters

import cats.implicits._
import com.github.skac112.klee.{ColorFun, Img, drawToFile, trivialColorFun}
import cats.Monad
import com.github.skac112.vgutils.Color
import shapeless._
import shapeless.ops._
import shapeless.ops.product.ToMap
import shapeless.syntax.std.product._

import scala.sys.process._

object Painter {
    case class RenderParams(minX: Double, maxX: Double, minY: Double, maxY: Double, nx: Int, ny: Int)    
}

import Painter._

abstract class Painter[P <: Product, M[_]: Monad](params: P, renderParams: RenderParams)(implicit toMap: ToMap.Aux[P, Symbol, Any])  {
  import Painter._
  def img: Img[Color, M]


  def paint(): M[Unit] = for {
    imgFile <- makeImgFile()
    dummy <- makeHtml(imgFile)
  } yield dummy

//    val paramsMap = params.toMap[Symbol, Any] map { case (k, v) => (k.name -> v) }
//    paramsMap foreach { case (k, v) => println(s"$k: $v") }
//    val git_hash = "git rev-parse --short HEAD".!!
//    println(git_hash.length())
//  }

  private def dateTimeStr: String = {
    import java.util.Calendar;
    import java.text.SimpleDateFormat;

    val form = new SimpleDateFormat("yy-MM-dd_hh::mm::ss");
    val temp = Calendar.getInstance();
    form.format(temp.getTime());
  }

  private def makeImgFile(): M[String] = {
    val git_hash = "git rev-parse --short HEAD".!!
    val datetime = dateTimeStr
    val class_name = getClass.getName
    val fileName = s"${class_name}_$datetime}_$git_hash"

    for {
      _ <- drawToFile[Color, M](img,
        trivialColorFun,
        fileName,
        renderParams.minX,
        renderParams.maxX,
        renderParams.minY,
        renderParams.maxY,
        renderParams.nx,
        renderParams.ny)
    } yield fileName
  }

  private def makeHtml(imgFile: String): M[Unit] = ???

}
