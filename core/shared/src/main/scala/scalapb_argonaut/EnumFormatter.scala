package scalapb_argonaut

import argonaut.Json

final case class EnumFormatter[T](
  writer: (Printer, T) => Json,
  parser: (Parser, Json) => Option[T]
)
