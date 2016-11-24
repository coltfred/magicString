package magicString

import shapeless._, ops.record._, shapeless.record._, shapeless.ops.hlist._
import shapeless.labelled.{ KeyTag, FieldType }
import scala.reflect.ClassTag

/**
 * Generate a List[MyResult] for any L which is an HList using `toStringRecLow`, `hnilToStringRecAgain` and `hconsToStringRecAgain`
 */
trait ToStringRecAgain[L] { def apply(l: L): List[SResult] }

trait LowPriorityToStringRecAgain {
  implicit def toStringRecLow[K <: Symbol, V: ToString, L <: HList](implicit
    kWit: Witness.Aux[K],
    tailToStringRecAgain: ToStringRecAgain[L]): ToStringRecAgain[FieldType[K, V] :: L] = new ToStringRecAgain[FieldType[K, V] :: L] {
    def apply(l: FieldType[K, V] :: L): List[SResult] = {
      SResult(Some(kWit.value), LeafResult[V](l.head.asInstanceOf[V])) +: tailToStringRecAgain(l.tail)
    }
  }
}

object ToStringRecAgain extends LowPriorityToStringRecAgain {
  implicit val hnilToStringRecAgain: ToStringRecAgain[HNil] = new ToStringRecAgain[HNil] {
    def apply(l: HNil): List[SResult] = Nil
  }

  implicit def hconsToStringRecAgain[K <: Symbol, V, R <: HList, T <: HList](implicit
    wit: Witness.Aux[K],
    gen: LabelledGeneric.Aux[V, R],
    tmrH: ToStringRecAgain[R],
    tmrT: ToStringRecAgain[T],
    ct: ClassTag[V]): ToStringRecAgain[FieldType[K, V] :: T] = new ToStringRecAgain[FieldType[K, V] :: T] {
    def apply(l: FieldType[K, V] :: T): List[SResult] = {
      SResult(Some(wit.value), LabelledResult(ct.runtimeClass.getName, tmrH(gen.to(l.head)))) +: tmrT(l.tail)
    }
  }

  implicit def listRec[A](implicit tsra: ToStringRecAgain[A]): ToStringRecAgain[List[A]] = new ToStringRecAgain[List[A]] {
    def apply(l: List[A]): List[SResult] = {
      l.flatMap(tsra(_))
    }
  }

  def toStringRec[A, L <: HList](a: A)(implicit
    gen: LabelledGeneric.Aux[A, L],
    tmr: ToStringRecAgain[L],
    ct: ClassTag[A]): MyResult = {
    LabelledResult(ct.runtimeClass.getName, tmr.apply(gen.to(a)))
  }
}
