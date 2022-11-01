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
import java.nio.file.{Paths, Files}
import java.nio.charset.StandardCharsets

object Painter {
    case class RenderParams(minX: Double, maxX: Double, minY: Double, maxY: Double, nx: Int, ny: Int)    
}

import Painter._

abstract class Painter[P <: Product, M[_]: Monad](params: P, renderParams: RenderParams)(implicit toMap: ToMap.Aux[P, Symbol, Any])  {
  import Painter._
  def img: Img[Color, M]

  def paint(): M[Unit] = {
    val file_name_base = fileNameBase
    for {
      _ <- makeImgFile()
      _ = makeHtmlFile()
    } yield None
  }

//    val paramsMap = params.toMap[Symbol, Any] map { case (k, v) => (k.name -> v) }
//    paramsMap foreach { case (k, v) => println(s"$k: $v") }
//    val git_hash = "git rev-parse --short HEAD".!!
//    println(git_hash.length())
//  }

  lazy val paramsString = params.toMap[Symbol, Any] map { case (k, v) => s"<li>${k.name}: <strong>$v</strong></li>" } reduce {_ + _}

  private lazy val dateTimeStr: String = {
    import java.util.Calendar;
    import java.text.SimpleDateFormat;

    val form = new SimpleDateFormat("yy-MM-dd_hh:mm:ss");
    val temp = Calendar.getInstance();
    form.format(temp.getTime());
  }

  private lazy val fileNameBase = s"${clName}_${dateTimeStr}_$gitHash"

  private def makeImgFile(): M[Unit] = {
    for {
      _ <- drawToFile[Color, M](img,
        trivialColorFun,
        s"$dir/$fileNameBase.png",
        renderParams.minX,
        renderParams.maxX,
        renderParams.minY,
        renderParams.maxY,
        renderParams.nx,
        renderParams.ny)
    } yield None
  }

  private def clName = getClass.getName

  private lazy val gitHash = "git rev-parse --short HEAD".!!.trim

  private lazy val dir = "src/main/scala/com/github/skac112/klee/paints"

  private def makeHtmlFile(): Unit = {
    val html = s"""<!doctype html><html><head><title>Run of klee library's painter</title></head><body>
     |<h1>Run of <strong>$clName</strong> painter</h1>
     |<p>
     |Time: <strong>$dateTimeStr</strong>Git hash: $gitHash
     |</p>
     |<h2>Painter parameters</h2>
     |<ul>$paramsString</ul>
     |<h3>Render parameters</h3>
     |<ul>
     |<li>X range: &lt;${renderParams.minX}; ${renderParams.maxX}&gt;</li>
     |<li>Y range: &lt;${renderParams.minY}; ${renderParams.maxY}&gt;</li>
     |<li>nx: ${renderParams.nx}, ny: ${renderParams.ny}</li>
     |</ul>
     |<img src="$fileNameBase.png">
     |</body></html>""".stripMargin.replaceAll("\n", " ")

    Files.write(Paths.get(s"$dir/$fileNameBase.html"), html.getBytes(StandardCharsets.UTF_8))
  }
}
