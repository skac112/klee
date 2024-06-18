package com.github.skac112.klee

import upickle.default._
import cats.{Id, Monad}
import cats.implicits._
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.vgutils.{ColorVector, Point}

package object serialize {
  given PointRW: ReadWriter[Point] = macroRW[Point]
  given BlackHoleRW: ReadWriter[BlackHole[ColorVector, Id]] = macroRW[BlackHole[ColorVector, Id]]

  implicit def imgTransRW: ReadWriter[ImgTrans.Simple[ColorVector, Id]] =
    readwriter[ujson.Value].bimap[ImgTrans.Simple[ColorVector, Id]](
    img_trans => if (img_trans.isInstanceOf[BlackHole[ColorVector, Id]]) {
      write[BlackHole[ColorVector, Id]](img_trans.asInstanceOf[BlackHole[ColorVector, Id]])
    } else {
      ujson.Arr(0)
    },
    json => ImgTrans.id[ColorVector, Id]
  )

  implicit def MonadRW[M[_]](implicit m: Monad[M]): ReadWriter[Monad[M]] = readwriter[ujson.Value].bimap[Monad[M]](
    m => ujson.Value(null),
    json => m)

  implicit def IdRW(implicit m: Monad[Id]): ReadWriter[Monad[Id]] = readwriter[ujson.Value].bimap[Monad[Id]](
    m => ujson.Value(null),
    json => m)

  class Sample(a: Int, b: String)
}


