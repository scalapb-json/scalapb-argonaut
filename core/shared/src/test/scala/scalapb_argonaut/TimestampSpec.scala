package scalapb_argonaut

import com.google.protobuf.timestamp.Timestamp
import jsontest.test.WellKnownTest
import argonaut.JsonParser.parse
import utest._
import EitherOps._

object TimestampSpec extends TestSuite {
  override val tests = Tests {
    "timestamp should serialize and parse correctly" - {
      val timestampJson =
        """{
          |  "timestamp": "2016-09-16T12:35:24.375123456Z"
          |}""".stripMargin
      val timestampProto =
        WellKnownTest(timestamp = Some(Timestamp(seconds = 1474029324, nanos = 375123456)))
      assert(JsonFormat.parser.fromJsonString[WellKnownTest](timestampJson) == timestampProto)
      assert(JsonFormat.printer.toJson(timestampProto) == parse(timestampJson).getOrError)
    }
  }
}
