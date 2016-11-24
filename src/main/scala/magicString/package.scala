
import shapeless._, ops.record._, shapeless.record._, shapeless.ops.hlist._
import shapeless.labelled.{ KeyTag, FieldType }
import scala.reflect.ClassTag

package object magicString {

  implicit class ToStringOps[A](val a: A) extends AnyVal {
    def magicString(implicit tsa: ToString[A]) = tsa.asString(a)
  }
}
