package magicString

trait ToString[A] {
  def asString(a: A): String
}

object ToString {

  import shapeless._, ops.record._, shapeless.record._, shapeless.ops.hlist._
  import shapeless.labelled.{ KeyTag, FieldType }
  import scala.reflect.ClassTag

  def apply[A](implicit ts: ToString[A]): ToString[A] = ts
  def create[A](f: A => String): ToString[A] = new ToString[A] {
    def asString(a: A): String = f(a)
  }

  def default[A]: ToString[A] = new ToString[A] {
    def asString(a: A): String = a.toString

  }

  implicit val toStringInt = default[Int]
  implicit val toStringLong = default[Long]
  implicit val toStringString = default[String]
  implicit val toStringDouble = default[Double]
  implicit def toStringList[A](implicit tsa: ToString[A]): ToString[List[A]] = create[List[A]] {
    case Nil => "[]"
    case l =>
      val innerString = l.map(tsa.asString).mkString(",")
      s"[$innerString]"
  }

  implicit def toStringForLabelledGeneric[A, L <: HList](implicit
    gen: LabelledGeneric.Aux[A, L],
    tmr: ToStringRec[L],
    ct: ClassTag[A]): ToString[A] = create[A] { a =>
    printNestedResult(ToStringRec.toStringRec(a))
  }

  private def printNestedResult(nestedResult: NestedResult): String = {
    val nestedLayer = nestedResult.unfix
    val lastPiece = nestedLayer.l match {
      case Nil => nestedLayer.className
      case l =>
        val childrenString = l.map(printNestedResult(_)).mkString(", ")

        s"""${nestedLayer.className}($childrenString)"""
    }
    nestedLayer.variableNameMaybe.map(_.name + "=").getOrElse("") + lastPiece
  }
}
