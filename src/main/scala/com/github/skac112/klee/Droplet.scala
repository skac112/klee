package com.github.skac112.klee

import cats.Monad
import com.github.skac112.vgutils.Point
import cats.implicits._

case class Droplet[I, M[_]: Monad](position: Point, load: M[I]) {
    // def widen[W >: I](ma: M[I]): M[W] = ma.flatMap[W](img_val => implicitly[Monad[M]].pure(img_val))
}
