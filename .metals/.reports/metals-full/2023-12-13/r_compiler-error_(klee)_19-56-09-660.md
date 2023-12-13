file://<WORKSPACE>/src/main/scala/com/github/skac112/klee/transforms/areas/Circle.scala
### scala.reflect.internal.Types$TypeError: illegal cyclic reference involving object implicits

occurred in the presentation compiler.

action parameters:
uri: file://<WORKSPACE>/src/main/scala/com/github/skac112/klee/transforms/areas/Circle.scala
text:
```scala
package com.github.skac112.klee.transforms.areas

import cats.Monad
import cats.implicits._
import com.github.skac112.klee.area.img.ImgArea
import com.github.skac112.klee.{Img, ImgPoint, ImgTrans, InstantImgPoint, LocalImgTrans}
import com.github.skac112.vgutils.{Color, Point}

case class Circle[I, M[_]](
                                   c: Point,
                                   r: Double,
                                   color: I,
                                   applyToAir: Boolean = false) extends LocalImgTrans[I, M] {
  override def area(implicit m: Monad[M]): ImgArea = com.github.skac112.klee.area.img.Circle(c, r)
  override def applyInArea(img: Img[I, M], ip: ImgPoint[I, M])(implicit m: Monad[M]): ImgPoint[I, M] = if (applyToAir || ip.land) {
    InstantImgPoint(ip.point, valueM(img, ip.point), ip.land)
  } else {
    ip
  }

  lazy val r2 = r*r

  protected def valueM(img: Img[I, M], ptM: M[Point])(implicit m: Monad[M]): M[I] = for {
    pt <- ptM
    mod2 = (pt - c).modulus2
    value <- if (mod2 <= r2) m.pure(color) else img(pt)
  } yield value
}

```



#### Error stacktrace:

```

```
#### Short summary: 

scala.reflect.internal.Types$TypeError: illegal cyclic reference involving object implicits