package com.github.skac112.klee
import com.github.skac112.vgutils.{Color, Point}

/**
  * Creates localized image transform from given base transform.
  */
case class LocalizedTrans(trans: ImgTrans, override val area: ImgArea) extends LocalImgTrans {
  override def applyInArea(img: Img, p: Point): Color = trans(img)(p)
}
