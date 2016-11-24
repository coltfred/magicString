package magicString

import shapeless._, ops.record._, shapeless.record._, shapeless.ops.hlist._
import shapeless.labelled.{ KeyTag, FieldType }
import scala.reflect.ClassTag

/**
 * Generate a List[MyResult] for any L which is an HList using `toStringRecLow`, `hnilToStringRec` and `hconsToStringRec`
 */
trait ToStringRec[L] { def apply(l: L): List[SResult] }

trait LowerPriorityToStringRec {
}

trait LowPriorityToStringRec extends LowerPriorityToStringRec {
  implicit def hconsWhereVIsNotLabelledGeneric[K <: Symbol, V, T <: HList](implicit
    wit: Witness.Aux[K],
    tmrH: ToStringRec[V],
    tmrT: ToStringRec[T],
    ct: ClassTag[V]): ToStringRec[FieldType[K, V] :: T] = new ToStringRec[FieldType[K, V] :: T] {
    def apply(l: FieldType[K, V] :: T): List[SResult] = {
      SResult(Some(wit.value), LabelledResult(ct.runtimeClass.getName, tmrH(l.head))) +: tmrT(l.tail)
    }
  }
}

object ToStringRec extends LowPriorityToStringRec {
  implicit val hnilToStringRec: ToStringRec[HNil] = new ToStringRec[HNil] {
    def apply(l: HNil): List[SResult] = Nil
  }

  implicit def hconsToStringRec[K <: Symbol, V, R <: HList, T <: HList](implicit
    wit: Witness.Aux[K],
    gen: LabelledGeneric.Aux[V, R],
    tmrH: ToStringRec[R],
    tmrT: ToStringRec[T],
    ct: ClassTag[V]): ToStringRec[FieldType[K, V] :: T] = new ToStringRec[FieldType[K, V] :: T] {
    def apply(l: FieldType[K, V] :: T): List[SResult] = {
      SResult(Some(wit.value), LabelledResult(ct.runtimeClass.getName, tmrH(gen.to(l.head)))) +: tmrT(l.tail)
    }
  }

  implicit def listRec[A](implicit tsra: ToStringRec[A]): ToStringRec[List[A]] = new ToStringRec[List[A]] {
    def apply(l: List[A]): List[SResult] = {
      l.flatMap(tsra(_))
    }
  }

  implicit def toStringRecFromToString[A](implicit tsa: ToString[A]): ToStringRec[A] = new ToStringRec[A] {
    def apply(a: A): List[SResult] = List(SResult(None, LeafResult(tsa.asString(a))))
  }

  def toStringRec[A, L <: HList](a: A)(implicit
    gen: LabelledGeneric.Aux[A, L],
    tmr: ToStringRec[L],
    ct: ClassTag[A]): MyResult = {
    LabelledResult(ct.runtimeClass.getName, tmr.apply(gen.to(a)))
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    case class Baz(notI: Int)
    case class Bar(first: String, second: Int, baz: Baz)
    case class BarBar(i: Int, s: String, l: List[Int])
    case class Foo(i: Int, singleBar: Bar, bars: List[Bar])
    val foo = Foo(
      1,
      Bar("im Single", 500, Baz(5000)),
      List(Bar("firstThing", 100, Baz(1000)), Bar("second", 200, Baz(2000)))
    )

    val bar = Bar("hello", 2, Baz(1))
    val barbar = BarBar(1, "barbar", List(1, 2, 3))
    val baz = Baz(1)
    val listBaz = List(Baz(1))

    //Not sure where this goes. It creates a ToStringRec for an A which is a labelled generic. It's like
    //the above `toStringRec`, but returning a ToStringRec instead of the MyResult.
    implicit def toStringRecForLabelledGeneric[A, L <: HList](implicit
      gen: LabelledGeneric.Aux[A, L],
      tmr: ToStringRec[L],
      ct: ClassTag[A]): ToStringRec[A] = new ToStringRec[A] {
      def apply(a: A): List[SResult] = List(SResult(None, LabelledResult(ct.runtimeClass.getName, tmr.apply(gen.to(a)))))
    }

    println(ToStringRec.toStringRec(bar))
    println(ToStringRec.toStringRec(barbar))
    println(ToStringRec.toStringRec(baz))
    println(ToStringRec.listRec(implicitly[ToStringRec[Baz]])(listBaz))
    //This line doesn't compile, but should. Diverging implicits.
    // println(ToStringRec.toStringRec(foo))

    ()
  }
}
