package com.github.skac112.klee.images.raster

import cats.Monad
import Raster.*
import com.github.skac112.vgutils.{ColorVector, Point}

case class ImmutableRaster[M[_]: Monad](
                            override val width: Int,
                            override val height: Int,
                            override val interpolation: Interpolation = Interpolation.Bilinear,       
                            override val pixels: Seq[Seq[M[ColorVector]]])
                            extends Raster[M]