package com.github.skac112.klee

import cats._
import cats.Monad
import cats.implicits._
import com.github.skac112.vgutils.Point

sealed trait ImgPoint[I, M[_]] {
  implicit val m: Monad[M]
  def point: M[Point]
  def color: M[I]
  def land: Boolean = true

  def bubbleUpMonad: M[PureImgPoint[I]] = for {
    col <- color
    pt <- point
  } yield new InstantPureImgPoint[I](pt, col, land)
}

final case class LandImgPoint[I, M[_]: Monad](img: Img[I, M], purePoint: Point) extends ImgPoint[I, M] {
  override lazy val m = implicitly[Monad[M]]
  override lazy val point = m.pure(purePoint)
  override lazy val color = img(purePoint)
}

final case class LazyColorImgPoint[I, M[_]: Monad](purePoint: Point, colorFun: () => M[I], override val land: Boolean = true) extends ImgPoint[I, M] {
  override lazy val m = implicitly[Monad[M]]
  override lazy val point = m.pure(purePoint)
  override lazy val color = colorFun()
}

final case class InstantImgPoint[I, M[_]: Monad](override val point: M[Point], color: M[I], override val land: Boolean = true) extends ImgPoint[I, M] {
  override val m = implicitly[Monad[M]]
}


