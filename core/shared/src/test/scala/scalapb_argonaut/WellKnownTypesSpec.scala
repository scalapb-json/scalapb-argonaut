package scalapb_argonaut

import com.google.protobuf.duration.Duration
import jsontest.test.WellKnownTest
import argonaut.JsonParser.parse
import utest._
import EitherOps._

object WellKnownTypesSpec extends TestSuite {
  val durationProto = WellKnownTest(duration = Some(Duration(146, 3455)))

  override val tests = Tests {
    "duration should serialize and parse correctly" - {
      val durationJson =
        """{
          |  "duration": "146.000003455s"
          |}""".stripMargin
      assert(JsonFormat.printer.toJson(durationProto) == parse(durationJson).getOrError)
      assert(JsonFormat.parser.fromJsonString[WellKnownTest](durationJson) == durationProto)
    }
  }
}
