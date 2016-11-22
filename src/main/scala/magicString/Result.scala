package magicString

case class Result[A](variableNameMaybe: Option[Symbol], className: String, l: List[A])
