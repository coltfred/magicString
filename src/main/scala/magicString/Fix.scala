package magicString

case class Fix[F[_]](unfix: F[Fix[F]])
