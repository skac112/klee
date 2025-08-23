package com.github.skac112.klee

import cats.*
import cats.Monad
import cats.implicits.*
import com.github.skac112.vgutils.{ColorVector, Point}

sealed trait ImgPoint[M[_]] {
  implicit val m: Monad[M]
  def point: M[Point]
  def colorO: Option[M[ColorVector]]
  def color = colorO.get
  def land: Boolean = true

  def bubbleUpMonad: M[PureImgPoint] = for {
    col <- colorO.get
    pt <- point
  } yield new InstantPureImgPoint(pt, col, land)
}

final case class LandImgPoint[M[_]: Monad](img: Img[M], purePoint: Point) extends ImgPoint[M] {
  val m = implicitly[Monad[M]]
  override lazy val point: M[Point] = m.pure(purePoint)
  override lazy val colorO: Option[M[ColorVector]] = Some(img(purePoint))
}

final case class LazyColorImgPoint[M[_]: Monad](purePoint: Point, 
                                                colorOFun: () => Option[M[ColorVector]], 
                                                override val land: Boolean = true) extends ImgPoint[M] {
  val m = implicitly[Monad[M]]
  override lazy val point: M[Point] = m.pure(purePoint)
  override lazy val colorO = colorOFun()
}

final case class InstantImgPoint[M[_]: Monad](override val point: M[Point],
                                              override val color: M[ColorVector],
                                              override val land: Boolean = true) extends ImgPoint[M] {
  override val colorO = Some(color)
  override val m = implicitly[Monad[M]]
}


