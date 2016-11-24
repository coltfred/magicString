package magicString

case class Result[A](variableNameMaybe: Option[Symbol], className: String, l: List[A])

case class SResult(s: Option[Symbol], r: MyResult)
trait MyResult

case class LabelledResult(typeName: String, l: List[SResult]) extends MyResult
// case class ComplexResult(typeName: String, l: List[MyResult]) extends MyResult
case class LeafResult[A: ToString](value: A) extends MyResult

// Foo(
//   l = List(
//     "foo"
//     "bar"),
//   b = Bar(m = 
//     Map(
//       99 -> Foo
//       )
//   )
// )
