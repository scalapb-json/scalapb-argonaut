package scalapb_argonaut

private[scalapb_argonaut] object ScalapbArgonautPlatform {
  def registerPlatformWriters(registry: FormatRegistry): FormatRegistry =
    registry

  def encodeToString(bytes: Array[Byte]): String =
    ScalapbArgonautBase64.getEncoder.encodeToString(bytes)

  def decode(str: String): Array[Byte] =
    ScalapbArgonautBase64.getDecoder.decode(str)
}
