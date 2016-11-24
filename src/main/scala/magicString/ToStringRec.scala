package magicString

import shapeless._, ops.record._, shapeless.record._, shapeless.ops.hlist._
import shapeless.labelled.{ KeyTag, FieldType }
import scala.reflect.ClassTag

/**
 * Generate a List[NestedResult] for any L which is an HList using `toStringRecLow`, `hnilToStringRec` and `hconsToStringRec`
 */
trait ToStringRec[L <: HList] { def apply(l: L): List[NestedResult] }

trait LowPriorityToStringRec {
  implicit def toStringRecLow[K <: Symbol, V: ToString, L <: HList](implicit
    kWit: Witness.Aux[K],
    tailToStringRec: ToStringRec[L]): ToStringRec[FieldType[K, V] :: L] = new ToStringRec[FieldType[K, V] :: L] {
    def apply(l: FieldType[K, V] :: L): List[NestedResult] = {

      val f = Fix[Result](Result(Some(kWit.value), ToString[V].asString(l.head.asInstanceOf[V]), Nil)) +: tailToStringRec(l.tail)
      println(f)
      f
    }
  }
}

object ToStringRec extends LowPriorityToStringRec {
  implicit val hnilToStringRec: ToStringRec[HNil] = new ToStringRec[HNil] {
    def apply(l: HNil): List[NestedResult] = Nil
  }

  implicit def hconsToStringRec[K <: Symbol, V, R <: HList, T <: HList](implicit
    wit: Witness.Aux[K],
    gen: LabelledGeneric.Aux[V, R],
    tmrH: ToStringRec[R],
    tmrT: ToStringRec[T],
    ct: ClassTag[V]): ToStringRec[FieldType[K, V] :: T] = new ToStringRec[FieldType[K, V] :: T] {
    def apply(l: FieldType[K, V] :: T): List[NestedResult] = {

      val f = Fix[Result](Result(Some(wit.value), ct.runtimeClass.getName, tmrH(gen.to(l.head)))) +: tmrT(l.tail)
      println("In recursive" + f)
      f
    }
  }

  def toStringRec[A, L <: HList](a: A)(implicit
    gen: LabelledGeneric.Aux[A, L],
    tmr: ToStringRec[L],
    ct: ClassTag[A]): NestedResult = {
    val f = Fix[Result](Result(None, ct.runtimeClass.getName, tmr(gen.to(a))))
    println("at top" + f)
    f
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    case class Baz(notI: Int)
    case class Bar(first: String, second: Int, baz: Baz)
    case class Foo(s: String, i: Int, singleBar: Bar, bars: List[Bar], baz: Baz)
    val foo = Foo(
      "Hello",
      1,
      Bar("im Single", 500, Baz(5000)),
      List(Bar("firstThing", 100, Baz(1000)), Bar("second", 200, Baz(2000))), Baz(6000)
    )

    // println(foo.magicString)

    val baz = Bar("hello", 2, Baz(1))
    println(ToStringRecAgain.toStringRec(foo))

    ()
  }
}
