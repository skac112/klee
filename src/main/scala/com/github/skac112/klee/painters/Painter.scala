package com.github.skac112.klee.painters

import com.github.skac112.klee.Img
import cats.Monad
import shapeless._
import shapeless.ops._
import shapeless.ops.product.ToMap
import shapeless.syntax.std.product._
import scala.sys.process._

object Painter {
    case class RenderParams(minX: Double, maxX: Double, minY: Double, maxY: Double, nx: Int, ny: Int)    
}

import Painter._

abstract class Painter[P <: Product, I, M[_]: Monad](params: P, renderParams: RenderParams)(implicit toMap: ToMap.Aux[P, Symbol, Any])  {
  import Painter._
  def img: Img[I, M]

  def paint() {
    val paramsMap = params.toMap[Symbol, Any] map { case (k, v) => (k.name -> v) }
    paramsMap foreach { case (k, v) => println(s"$k: $v") }
    val git_hash = "git rev-parse --short HEAD".!!
    println(git_hash.length())
  }
}
