package com.github.skac112.klee

import cats._
import cats.Monad
import cats.implicits._
import com.github.skac112.vgutils.Point

sealed trait ImgPoint[I, M[_]] {
  implicit val m: Monad[M]
  def point: M[Point]
  def colorO: Option[M[I]]
  def color = colorO.get
  def land: Boolean = true

  def bubbleUpMonad: M[PureImgPoint[I]] = for {
    col <- colorO.get
    pt <- point
  } yield new InstantPureImgPoint[I](pt, col, land)
}

final case class LandImgPoint[I, M[_]: Monad](img: Img[I, M], purePoint: Point) extends ImgPoint[I, M] {
  val m = implicitly[Monad[M]]
  override lazy val point: M[Point] = m.pure(purePoint)
  override lazy val colorO: Option[M[I]] = Some(img(purePoint))
}

final case class LazyColorImgPoint[I, M[_]: Monad](purePoint: Point, colorOFun: () => Option[M[I]], override val land: Boolean = true) extends ImgPoint[I, M] {
  val m = implicitly[Monad[M]]
  override lazy val point: M[Point] = m.pure(purePoint)
  override lazy val colorO = colorOFun()
}

final case class InstantImgPoint[I, M[_]: Monad](override val point: M[Point], override val color: M[I], override val land: Boolean = true) extends ImgPoint[I, M] {
  override val colorO = Some(color)
  override val m = implicitly[Monad[M]]
}


