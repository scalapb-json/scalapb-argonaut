package scalapb_argonaut

import argonaut.Json
import com.google.protobuf.timestamp.Timestamp
import scalapb_json.JsonFormatException
import scalapb_json.Timestamps

private[scalapb_argonaut] object ScalapbArgonautPlatform {
  def registerPlatformWriters(registry: FormatRegistry): FormatRegistry = {
    registry.registerWriter(
      (t: Timestamp) => Json.jString(Timestamps.writeTimestamp(t)), {
        _.string match {
          case Some(str) =>
            Timestamps.parseTimestamp(str)
          case _ =>
            throw new JsonFormatException("Expected a string.")
        }
      }
    )
  }
}
