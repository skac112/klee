package com.github.skac112.klee

import upickle.default.*
import cats.{Id, Monad}
import cats.implicits.*
import com.github.skac112.vgutils.{Bounds, ColorVector, Point}
import com.github.skac112.klee.images.raster.*
import com.github.skac112.klee.images.raster.Raster.Interpolation

package object serialize {
  given PointRW: ReadWriter[Point] = macroRW[Point]
  given ColorVectorRW: ReadWriter[ColorVector] = macroRW[ColorVector]
  given BoundsRW: ReadWriter[Bounds] = readwriter[ujson.Value].bimap[Bounds](
    b => ujson.Obj(
      "tl" -> writeJs(b.tl),
      "br" -> writeJs(b.br)
    ),
    json =>
      val obj = json.obj
      val tl = read[Point](obj("tl"))
      val br = read[Point](obj("br"))
      com.github.skac112.vgutils.Bounds(tl, br)
  )

  // Ręczny ReadWriter dla BlackHole[Id] (serializujemy tylko pola)
  given BlackHoleRW: ReadWriter[com.github.skac112.klee.transforms.displacers.BlackHole[Id]] =
    readwriter[ujson.Value].bimap[com.github.skac112.klee.transforms.displacers.BlackHole[Id]](
      bh => ujson.Obj(
        "c" -> writeJs(bh.c),
        "rotation" -> ujson.Num(bh.rotation),
        "rotationDecay" -> ujson.Num(bh.rotationDecay),
        "scaling" -> ujson.Num(bh.scaling),
        "scalingDecay" -> ujson.Num(bh.scalingDecay),
        "areaRadius" -> ujson.Num(bh.areaRadius)
      ),
      json =>
        val obj = json.obj
        val c = read[Point](obj("c"))
        val rotation = obj("rotation").num
        val rotationDecay = obj("rotationDecay").num
        val scaling = obj("scaling").num
        val scalingDecay = obj("scalingDecay").num
        val areaRadius = obj.get("areaRadius").map(_.num).getOrElse(0.0)
        com.github.skac112.klee.transforms.displacers.BlackHole[Id](c, rotation, rotationDecay, scaling, scalingDecay, areaRadius)
    )

  given AxisGridRW: ReadWriter[com.github.skac112.klee.area.imgpt.AxisGrid[Id]] =
    readwriter[ujson.Value].bimap[com.github.skac112.klee.area.imgpt.AxisGrid[Id]](
      axisGrid => ujson.Obj(
        "leftTop" -> ujson.Obj(
          "x" -> ujson.Num(axisGrid.leftTop.x),
          "y" -> ujson.Num(axisGrid.leftTop.y)
        ),
        "nx" -> ujson.Num(axisGrid.nx),
        "ny" -> ujson.Num(axisGrid.ny),
        "dx" -> ujson.Num(axisGrid.dx),
        "dy" -> ujson.Num(axisGrid.dy)
      ),
      json => {
        val obj = json.obj
        val defaultColorFunFun: Int => () => Option[Id[ColorVector]] = _ => () => None
        val leftTopObj = obj("leftTop").obj
        com.github.skac112.klee.area.imgpt.AxisGrid[Id](
          Point(leftTopObj("x").num, leftTopObj("y").num),
          obj("nx").num.toInt,
          obj("ny").num.toInt,
          obj("dx").num,
          obj("dy").num,
          defaultColorFunFun
        )
      }
    )

  // ReadWritery dla ImgPoint i PureImgPoint (Id) - usunąłem 'lazy' gdzie występowało
  given PureImgPointRW: ReadWriter[com.github.skac112.klee.PureImgPoint] = readwriter[ujson.Value].bimap[com.github.skac112.klee.PureImgPoint](
    pp => pp match
      case com.github.skac112.klee.InstantPureImgPoint(point, color, land) =>
        ujson.Obj("type" -> ujson.Str("InstantPureImgPoint"), "point" -> writeJs(point), "color" -> writeJs(color), "land" -> ujson.Bool(land))
  ,
    json => json("type").str match
      case "InstantPureImgPoint" =>
        val obj = json.obj
        val p = read[com.github.skac112.vgutils.Point](obj("point"))
        val c = read[com.github.skac112.vgutils.ColorVector](obj("color"))
        val l = obj.get("land").map(_.bool).getOrElse(true)
        com.github.skac112.klee.InstantPureImgPoint(p, c, l)
  )

  given ImgPointIdRW: ReadWriter[com.github.skac112.klee.ImgPoint[Id]] = readwriter[ujson.Value].bimap[com.github.skac112.klee.ImgPoint[Id]](
    ip => ip match
      case com.github.skac112.klee.InstantImgPoint(pointM, colorM, land) =>
        val p = pointM.asInstanceOf[Id[com.github.skac112.vgutils.Point]]
        val c = colorM.asInstanceOf[Id[com.github.skac112.vgutils.ColorVector]]
        ujson.Obj("type" -> ujson.Str("InstantImgPoint"), "point" -> writeJs(p), "color" -> writeJs(c), "land" -> ujson.Bool(land))
      case com.github.skac112.klee.LazyColorImgPoint(purePoint, colorOFun, land) =>
        ujson.Obj("type" -> ujson.Str("LazyColorImgPoint"), "point" -> writeJs(purePoint), "land" -> ujson.Bool(land))
      case com.github.skac112.klee.LandImgPoint(img, purePoint) =>
        ujson.Obj("type" -> ujson.Str("LandImgPoint"), "point" -> writeJs(purePoint))
  ,
    json => json("type").str match
      case "InstantImgPoint" =>
        val obj = json.obj
        val p = read[com.github.skac112.vgutils.Point](obj("point"))
        val c = read[com.github.skac112.vgutils.ColorVector](obj("color"))
        val l = obj.get("land").map(_.bool).getOrElse(true)
        com.github.skac112.klee.InstantImgPoint[Id](p, c, l)
      case "LazyColorImgPoint" =>
        val obj = json.obj
        val p = read[com.github.skac112.vgutils.Point](obj("point"))
        val defaultColorOFun: () => Option[Id[com.github.skac112.vgutils.ColorVector]] = () => None
        val l = obj.get("land").map(_.bool).getOrElse(true)
        com.github.skac112.klee.LazyColorImgPoint[Id](p, defaultColorOFun, l)
      case "LandImgPoint" =>
        val obj = json.obj
        val p = read[com.github.skac112.vgutils.Point](obj("point"))
        val stubImg: com.github.skac112.klee.Img[Id] = new com.github.skac112.klee.Img[Id] {
          def apply(p: com.github.skac112.vgutils.Point)(using m: Monad[Id]) = implicitly[Monad[Id]].pure(com.github.skac112.vgutils.ColorVector(0,0,0))
        }
        com.github.skac112.klee.LandImgPoint[Id](stubImg, p)
  )

  given ImgPointsIdRW: ReadWriter[com.github.skac112.klee.ImgPoints[Id]] = readwriter[ujson.Value].bimap[com.github.skac112.klee.ImgPoints[Id]](
    seq => ujson.Arr(seq.map(ip => writeJs(ip)).toArray: _*),
    json => json.arr.toSeq.map(elem => read[com.github.skac112.klee.ImgPoint[Id]](elem))
  )

  given ImgAreaRW: ReadWriter[com.github.skac112.klee.area.img.ImgArea] = readwriter[ujson.Value].bimap[
    com.github.skac112.klee.area.img.ImgArea](
    area => area match
      case com.github.skac112.klee.area.img.EmptyArea() => ujson.Obj("type" -> ujson.Str("EmptyArea"))
      case ar: com.github.skac112.klee.area.img.AxisRect => ujson.Obj("type" -> ujson.Str("AxisRect"), "leftTop" -> writeJs(ar.leftTop), "width" -> ujson.Num(ar.width), "height" -> ujson.Num(ar.height))
      case ar: com.github.skac112.klee.area.img.BoundsArea => ujson.Obj("type" -> ujson.Str("BoundsArea"), "givenBounds" -> writeJs(ar.givenBounds))
      case ar: com.github.skac112.klee.area.img.MultiPartArea => ujson.Obj("type" -> ujson.Str("MultiPartArea"), "parts" -> ujson.Arr(ar.parts.toSeq.map(p => writeJs(p)).toArray: _*))
      case _ => ujson.Obj("type" -> ujson.Str("UnknownArea"))
  ,
    json => json("type").str match
      case "EmptyArea" => com.github.skac112.klee.area.img.EmptyArea()
      case "AxisRect" =>
        val obj = json.obj
        val lt = read[com.github.skac112.vgutils.Point](obj("leftTop"))
        val w = obj("width").num
        val h = obj("height").num
        com.github.skac112.klee.area.img.AxisRect(lt, w, h)
      case "BoundsArea" =>
        val obj = json.obj
        val b = read[com.github.skac112.vgutils.Bounds](obj("givenBounds"))
        com.github.skac112.klee.area.img.BoundsArea(b)
      case "MultiPartArea" =>
        val obj = json.obj
        val parts = obj("parts").arr.toSeq.map(pj => read[com.github.skac112.klee.area.img.ImgArea](pj))
        com.github.skac112.klee.area.img.MultiPartArea(parts.toSet)
      case _ => com.github.skac112.klee.area.img.EmptyArea()
  )

  // Finally: ReadWriter dla ImgPtArea[Id]
  given imgPtAreaRW: ReadWriter[com.github.skac112.klee.area.imgpt.ImgPtArea[Id]] =
    readwriter[ujson.Value].bimap[com.github.skac112.klee.area.imgpt.ImgPtArea[Id]](
      area => area match
        case com.github.skac112.klee.area.imgpt.EmptyArea() => ujson.Obj("type" -> ujson.Str("area.imgpt.EmptyArea"))
        case ag: com.github.skac112.klee.area.imgpt.AxisGrid[Id] => ujson.Obj("type" -> ujson.Str("area.imgpt.AxisGrid"), "data" -> writeJs(ag))
        case ba: com.github.skac112.klee.area.imgpt.BoundsArea[Id] => ujson.Obj("type" -> ujson.Str("area.imgpt.BoundsArea"), "givenBounds" -> writeJs(ba.givenBounds), "imgPoints" -> writeJs(ba.imgPoints))
        case qa: com.github.skac112.klee.area.imgpt.QuickPtArea[Id] => ujson.Obj("type" -> ujson.Str("area.imgpt.QuickPtArea"), "area" -> writeJs(qa.area), "imgPoints" -> writeJs(qa.imgPoints))
        case ma: com.github.skac112.klee.area.imgpt.MultiPartArea[Id] => ujson.Obj("type" -> ujson.Str("area.imgpt.MultiPartArea"), "parts" -> ujson.Arr(ma.parts.map(p => writeJs(p)).toArray: _*))
        case _ => ujson.Obj("type" -> ujson.Str("Unknown"))
    ,
      json => json("type").str match
        case "area.imgpt.EmptyArea" => com.github.skac112.klee.area.imgpt.EmptyArea[Id]()
        case "area.imgpt.AxisGrid" => read[com.github.skac112.klee.area.imgpt.AxisGrid[Id]](json("data"))
        case "area.imgpt.BoundsArea" =>
          val givenB = read[com.github.skac112.vgutils.Bounds](json("givenBounds"))
          val pts = read[com.github.skac112.klee.ImgPoints[Id]](json("imgPoints"))
          com.github.skac112.klee.area.imgpt.BoundsArea[Id](givenB, pts)
        case "area.imgpt.QuickPtArea" =>
          val ar = read[com.github.skac112.klee.area.img.ImgArea](json("area"))
          val pts = read[com.github.skac112.klee.ImgPoints[Id]](json("imgPoints"))
          com.github.skac112.klee.area.imgpt.QuickPtArea[Id](pts, ar)
        case "area.imgpt.MultiPartArea" =>
          val parts = json("parts").arr.toSeq.map(pj => read[com.github.skac112.klee.area.imgpt.ImgPtArea[Id]](pj))
          com.github.skac112.klee.area.imgpt.MultiPartArea[Id](parts)
        case _ => com.github.skac112.klee.area.imgpt.EmptyArea[Id]()
    )

  // ReadWriter dla ImgTrans[Id] - obsługujemy tylko transforms.displacers.BlackHole
  given imgTransRW: ReadWriter[com.github.skac112.klee.ImgTrans[Id]] = {
    readwriter[ujson.Value].bimap[com.github.skac112.klee.ImgTrans[Id]](
      img_trans => img_trans match
        case bh: com.github.skac112.klee.transforms.displacers.BlackHole[Id] =>
          ujson.Obj("type" -> ujson.Str("transforms.displacers.BlackHole"), "data" -> writeJs(bh))
        case _ => ujson.Obj("type" -> ujson.Str("Unknown"))
      ,
      json => json("type").str match
        case "transforms.displacers.BlackHole" => read[com.github.skac112.klee.transforms.displacers.BlackHole[Id]](json("data"))
        case _ => com.github.skac112.klee.ImgTrans.id[Id]
    )
  }

  // ReadWriter dla enumu Interpolation (serializujemy jako string)
  given InterpolationRW: ReadWriter[com.github.skac112.klee.images.raster.Raster.Interpolation] =
    readwriter[ujson.Value].bimap[com.github.skac112.klee.images.raster.Raster.Interpolation](
      interp => ujson.Str(interp.toString),
      json => json.str match
        case "Nearest" => com.github.skac112.klee.images.raster.Raster.Interpolation.Nearest
        case "Bilinear" => com.github.skac112.klee.images.raster.Raster.Interpolation.Bilinear
        case "Bicubic" => com.github.skac112.klee.images.raster.Raster.Interpolation.Bicubic
        case _ => com.github.skac112.klee.images.raster.Raster.Interpolation.Bilinear
    )

  given imgRW: ReadWriter[com.github.skac112.klee.Img[Id]] = readwriter[ujson.Value].bimap[com.github.skac112.klee.Img[Id]](
    img => img match {
      case MutableRaster(width, height, initImg, interpolation) =>
        ujson.Obj(
          "type" -> ujson.Str("images.rasterMutableRaster"),
          "width" -> ujson.Num(width),
          "height" -> ujson.Num(height),
          "initImg" -> writeJs(initImg),
          "interpolation" -> ujson.Str(interpolation.toString)
        )

      case ImmutableRaster(width, height, interpolation, pixels) =>
        ujson.Obj(
          "type" -> ujson.Str("images.rasterImmutableRaster"),
          "width" -> ujson.Num(width),
          "height" -> ujson.Num(height),
          "pixels" -> ujson.Arr(pixels.map(row => ujson.Arr(row.map(pixel => writeJs(pixel)).toArray: _*)).toArray: _*),
          "interpolation" -> ujson.Str(interpolation.toString)
        )
    },
    json => json("type").str match
      case "images.rasterMutableRaster" =>
        val obj = json.obj
        val width = obj("width").num.toInt
        val height = obj("height").num.toInt
        val initImg = read[com.github.skac112.klee.Img[Id]](obj("initImg"))
        val interpolation = obj("interpolation").str match
          case "Nearest" => com.github.skac112.klee.images.raster.Raster.Interpolation.Nearest
          case "Bilinear" => com.github.skac112.klee.images.raster.Raster.Interpolation.Bilinear
          case "Bicubic" => com.github.skac112.klee.images.raster.Raster.Interpolation.Bicubic
          case _ => com.github.skac112.klee.images.raster.Raster.Interpolation.Bilinear
        MutableRaster(width, height, interpolation, initImg)
        
      case "images.rasterImmutableRaster" =>
        val obj = json.obj
        val width = obj("width").num.toInt
        val height = obj("height").num.toInt
        val interpolation = read[Interpolation](obj("interpolation"))
        val pixels = obj("pixels").arr.toSeq.map(row => row.arr.toSeq.map(pixel => read[Id[com.github.skac112.vgutils.ColorVector]](pixel)))
        ImmutableRaster(width, height, interpolation, pixels)
  )

  //  // Minimalny ReadWriter dla ImgPtArea[Id] — tylko tag typu (można rozbudować później)
//  given imgPtAreaRW: ReadWriter[com.github.skac112.klee.area.imgpt.ImgPtArea[Id]] =
//    readwriter[String].bimap[com.github.skac112.klee.area.imgpt.ImgPtArea[Id]](
//      area => area match
//        case com.github.skac112.klee.area.imgpt.EmptyArea() => "EmptyArea"
//        case _ => "EmptyArea"
//      ,
//      tag => tag match
//        case "EmptyArea" => com.github.skac112.klee.area.imgpt.EmptyArea[Id]()
//        case _ => com.github.skac112.klee.area.imgpt.EmptyArea[Id]()
//    )

  class Sample(a: Int, b: String)
}
