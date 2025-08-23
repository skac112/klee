package com.github.skac112.klee

import upickle.default.*
import cats.{Id, Monad}
import cats.implicits.*
import com.github.skac112.klee.images.Fill
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.vgutils.{ColorVector, Point}

package object serialize {
  given PointRW: ReadWriter[Point] = macroRW[Point]
  given BlackHoleRW: ReadWriter[BlackHole[Id]] = macroRW[BlackHole[Id]]
  given ColorVectorRW: ReadWriter[ColorVector] = macroRW[ColorVector]
  given FillRW: ReadWriter[Fill[Id]] = macroRW[Fill[Id]]
  
  given imgTransRW: ReadWriter[ImgTrans.Simple[Id]] =
    readwriter[ujson.Obj].bimap[ImgTrans.Simple[Id]](
      img_trans => img_trans match
        case t @ BlackHole[Id] (c, rotation, rotationDecay, scaling, scalingDecay, areaRadius) =>
          Map("type" -> "transforms.displacers.BlackHole", "data" -> write[BlackHole[Id]](t))
        case _ =>
          Map() // Placeholder for other ImgTrans types, can be extended later
  ,
  //      if (img_trans.isInstanceOf[BlackHole[Id]]) {
  //      write[BlackHole[Id]](img_trans.asInstanceOf[BlackHole[Id]])

      json_obj => json_obj("type").toString match
          case "transforms.displacers.BlackHole" =>
            read[BlackHole[Id]](json_obj("data"))
          case _ => ImgTrans.id[Id] // Default case, can be extended later
    )

  implicit def MonadRW[M[_]](implicit m: Monad[M]): ReadWriter[Monad[M]] = readwriter[ujson.Value].bimap[Monad[M]](
    m => ujson.Value(null),
    json => m)

  implicit def IdRW(implicit m: Monad[Id]): ReadWriter[Monad[Id]] = readwriter[ujson.Value].bimap[Monad[Id]](
    m => ujson.Value(null),
    json => m)

  class Sample(a: Int, b: String)
}


