package thinString

import shapeless._, ops.record._, shapeless.record._, shapeless.ops.hlist._
import shapeless.labelled.{ KeyTag, FieldType }
import scala.reflect.ClassTag

case class NestedResult(variableNameMaybe: Option[Symbol], className: String, l: List[NestedResult]) {
  def eval: String = {
    val lastPiece = l match {
      case Nil => className
      case l => s"""$className(${l.map(_.eval).mkString(", ")})"""
    }
    variableNameMaybe.map(_.name + "=").getOrElse("") + lastPiece
  }
}

object MakeString {
  trait ToStringRec[L <: HList] { def apply(l: L): List[NestedResult] }

  trait LowPriorityToStringRec {
    implicit def toStringRecLow[K <: Symbol, V, L <: HList](implicit
      kWit: Witness.Aux[K],
      tailToStringRec: ToStringRec[L]): ToStringRec[FieldType[K, V] :: L] = new ToStringRec[FieldType[K, V] :: L] {
      def apply(l: FieldType[K, V] :: L): List[NestedResult] = {
        NestedResult(Some(kWit.value), l.head.toString, Nil) +: tailToStringRec(l.tail)
      }
    }

  }

  object ToStringRec extends LowPriorityToStringRec {
    implicit val hnilToStringRec: ToStringRec[HNil] = new ToStringRec[HNil] {
      def apply(l: HNil): List[NestedResult] = Nil
    }

    implicit def hconsToStringRec0[K <: Symbol, V, R <: HList, T <: HList](implicit
      wit: Witness.Aux[K],
      gen: LabelledGeneric.Aux[V, R],
      tmrH: ToStringRec[R],
      tmrT: ToStringRec[T],
      ct: ClassTag[V]): ToStringRec[FieldType[K, V] :: T] = new ToStringRec[FieldType[K, V] :: T] {
      def apply(l: FieldType[K, V] :: T): List[NestedResult] =
        NestedResult(Some(wit.value), ct.runtimeClass.getName, tmrH(gen.to(l.head))) +: tmrT(l.tail)
    }

    def toStringRec[A, L <: HList](a: A)(implicit
      gen: LabelledGeneric.Aux[A, L],
      tmr: ToStringRec[L],
      ct: ClassTag[A]): NestedResult = NestedResult(None, ct.runtimeClass.getName, tmr(gen.to(a)))
  }

  implicit class ToStringRecOps[A](val a: A) extends AnyVal {

    def showMe[L <: HList](implicit
      gen: LabelledGeneric.Aux[A, L],
      tmr: ToStringRec[L],
      ct: ClassTag[A]): String = {
      val rootResult = ToStringRec.toStringRec(a)
      rootResult.eval
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    import MakeString._
    case class Bar(first: String, second: Int)
    case class Foo(s: String, i: Int, d: Double, bar: Bar)
    val x = Foo("Hello", 1, 2.7, Bar("firstThing", 100)).showMe
    println(x)
    ()

  }
}
