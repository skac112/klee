package com.github.skac112.klee.transtrans

import cats.*
import cats.implicits.*
import com.github.skac112.klee.transforms.displacers.{Displacer, QuickDisplacer}
import com.github.skac112.klee.transforms.sprite.*
import com.github.skac112.klee.{Img, ImgPoint, ImgTrans, InstantImgPoint}
import com.github.skac112.vgutils.{ColorVector, Point}
import com.github.skac112.vgutils.transform.Affine
import com.github.skac112.klee.flows.vectormaps.VectorMap
import cats.implicits.*
import cats.*
import com.github.skac112.klee.transcomb.sprite.Composition

case class AffineSprite[M[_]](affine: Affine)(using Monad[M]) extends ImgTransTrans[M, Sprite[M]]:
  private type applyInAreaFun = (Img[M], ImgPoint[M]) => ImgPoint[M]

  private def newApplyInAreaFun(baseApplyInArea: applyInAreaFun): applyInAreaFun = (img: Img[M], ip: ImgPoint[M]) =>
    val p_m: M[Point] = ip.point
    // transformacja odwrotna
    val affine_inv = affine.inv
    // korekta transformacji afinicznej odwrotnej - odjęcie położenia początkowego (zgodnie z logiką displacera)
    val to_base_disp_trans = Affine(
      affine_inv.a - 1.0,
      affine_inv.b,
      affine_inv.c,
      affine_inv.d - 1.0,
      affine_inv.e,
      affine_inv.f)

    // korekta zadanej transformacji afinicznej - odjęcie położenia początkowego (zgodnie z logiką displacera)
    val from_base_disp_trans = Affine(
      affine.a - 1.0,
      affine.b,
      affine.c,
      affine.d - 1.0,
      affine.e,
      affine.f)

    val to_base_vm = VectorMap.from(to_base_disp_trans)
    val from_base_vm = VectorMap.from(from_base_disp_trans)
    // displacer transformacji odwrotnej - zgodnie z logiką displacera używa transformacji odwrotnej do odwrotnej, czyl
    // oryginalnej
    val to_base_disp = QuickDisplacer[M](from_base_vm)
    // displacer zadanej transformacji afinicznej - zgodnie z logiką displacera używa transformacji odwrotnej
    val from_base_disp = QuickDisplacer[M](to_base_vm)
    // obraz po transformacji odwrotnej
    val to_base_img = to_base_disp(img)
    // punkt obrazu we współrzędnych bazowych - zmieniane są tylko współrzędne punktu
    val to_base_ip: ImgPoint[M] = transformIp(to_base_img, ip, to_base_disp)
    // punkt obrazu po transformacji bazowej
    val base_trans_ip = baseApplyInArea(to_base_img, to_base_ip)
    // punkt obrazu po transformacji zadanej - zmieniane są tylko współrzędne punktu
    transformIp(img, base_trans_ip, from_base_disp)

  override def apply(sprite: Sprite[M]): Sprite[M] = sprite match {
    case qss @ QuickSimpleSprite(sprite_area, apply_in_area_fun) => qss.copy(
      sprite_area.transform(affine),
      newApplyInAreaFun(apply_in_area_fun))

    case qus @ QuickUnitSprite(apply_in_area_fun) => QuickSimpleSprite(
      qus.spriteArea.transform(affine),
      newApplyInAreaFun(qus.applyInAreaFun))
    
    case Composition(elements) => Composition[M](elements.map(apply))
  }

  private def transformIp(img: Img[M], ip: ImgPoint[M], displacer: Displacer[M])(using m: Monad[M]): ImgPoint[M] = {
    val dispM: M[Point] = for
      pt <- ip.point
      disp <- displacer.displacement.apply(pt)
    yield disp

    val dpM = ptSumM(ip.point, dispM)
    // punkty land i air traktowane są tak samo - zmieniane są tylko współrzędne punktu
    InstantImgPoint(ip.point, imgForPtM(img, dpM), ip.land)
  }

  private def ptSumM(ptM1: M[Point], ptM2: M[Point])(implicit m: Monad[M]) = for
    pt1 <- ptM1
    pt2 <- ptM2
  yield pt1 + pt2

  private def transCoordIp(ip: ImgPoint[M], transCoord: Point => Point)(implicit m: Monad[M]): ImgPoint[M] = InstantImgPoint(
    ip.point.map(transCoord),
    ip.color,
    ip.land
  )

  private def imgForPtM(img: Img[M], ptM: M[Point])(implicit m: Monad[M]): M[ColorVector] = for
    pt <- ptM
    value <- img(pt)
  yield value
