package com.github.skac112.klee

import upickle.default.*
import cats.{Id, Monad}
import cats.implicits.*
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.vgutils.{ColorVector, Point}

package object serialize {
  given PointRW: ReadWriter[Point] = macroRW[Point]
  given BlackHoleRW: ReadWriter[BlackHole[ColorVector, Id]] = macroRW[BlackHole[ColorVector, Id]]
  given FillRW: ReadWriter[Fill[ColorVector, Id]] = macroRW[Fill[ColorVector, Id]]
  
  given imgTransRW: ReadWriter[ImgTrans.Simple[ColorVector, Id]] =
    readwriter[ujson.Obj].bimap[ImgTrans.Simple[ColorVector, Id]](
      img_trans => img_trans match
        case t @ BlackHole[ColorVector, Id] (c, rotation, rotationDecay, scaling, scalingDecay, areaRadius) =>
          Map("type" -> "transforms.displacers.BlackHole", "data" -> write[BlackHole[ColorVector, Id]](t))
        case _ =>
          Map() // Placeholder for other ImgTrans types, can be extended later
  ,
  //      if (img_trans.isInstanceOf[BlackHole[ColorVector, Id]]) {
  //      write[BlackHole[ColorVector, Id]](img_trans.asInstanceOf[BlackHole[ColorVector, Id]])

      json_obj => json_obj("type").toString match
          case "transforms.displacers.BlackHole" =>
            read[BlackHole[ColorVector, Id]](json_obj("data"))
          case _ => ImgTrans.id[ColorVector, Id] // Default case, can be extended later
    )

  implicit def MonadRW[M[_]](implicit m: Monad[M]): ReadWriter[Monad[M]] = readwriter[ujson.Value].bimap[Monad[M]](
    m => ujson.Value(null),
    json => m)

  implicit def IdRW(implicit m: Monad[Id]): ReadWriter[Monad[Id]] = readwriter[ujson.Value].bimap[Monad[Id]](
    m => ujson.Value(null),
    json => m)

  class Sample(a: Int, b: String)
}


