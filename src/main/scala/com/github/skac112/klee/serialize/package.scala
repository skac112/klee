package com.github.skac112.klee

import upickle.default._
import cats.{Id, Monad}
import cats.implicits._
import com.github.skac112.klee.transforms.displacers.BlackHole
import com.github.skac112.vgutils.{ColorVector, Point}
import shapeless._
import shapeless.record._
import shapeless.ops.record._
import labelled.{FieldType, field}
import shapeless.Lazy.apply

package object serialize {
  implicit val PointRW = macroRW[Point]
  implicit val BlackHoleRW = macroRW[BlackHole[ColorVector, Id]]

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

  trait FromMap[L <: HList] {
    def apply(m: Map[String, Any]): Option[L]
  }

  object FromMap {
    implicit val hnilFromMap: FromMap[HNil] = new FromMap[HNil] {
      def apply(m: Map[String, Any]): Option[HNil] = Some(HNil)
    }

    implicit def hconsFromMap[K <: Symbol, V, T <: HList](implicit
                                                          witness: Witness.Aux[K],
                                                          typeable: Typeable[V],
                                                          fromMapT: FromMap[T]): FromMap[FieldType[K, V] :: T] = new FromMap[FieldType[K, V] :: T] {
      def apply(m: Map[String, Any]): Option[FieldType[K, V] :: T] = for {
        v <- m.get(witness.value.name.toString)
        r <- typeable.cast(v)
        t <- fromMapT(m)
      } yield field[K][V](r) :: t
    }
  }

  class Helper[A] {
    def from[R <: HList](m: Map[String, Any])(implicit
                                              gen: LabelledGeneric.Aux[A, R],
                                              fromMap: FromMap[R]): Option[A] = {
      fromMap(m).map(gen.from(_))
    }

    def to[R <: HList](cc: A)(implicit
                                gen: LabelledGeneric.Aux[A, R],
                                toMap: ToMap[R]): Map[String, Any] = {
      toMap(gen.to(cc)).map[String, Any]{ kv => (kv._1.toString, kv._2)}
    }
  }

  object Helper {
    def to[A]: Helper[A] = new Helper[A]
  }

//  def bhmap[M[_]: Monad](map: Map[String, Any]) = Helper.to[BlackHole[ColorVector, M]].from(map)

//  implicit def caseClassReadWriter[CC](implicit gen: LabelledGeneric[CC]): ReadWriter[CC] = {
//    val helper = Helper.to[CC]
////    implicit val g = gen.
//    readwriter[ujson.Value].bimap[CC](
//      cc => write[Map[String, Any](helper.to(cc)),
//      map => helper.from(map).get)
//  }

}


