package magicString

case class SResult(s: Option[Symbol], r: MyResult)
trait MyResult

case class LabelledResult(typeName: String, l: List[SResult]) extends MyResult
case class LeafResult[A: ToString](value: A) extends MyResult
