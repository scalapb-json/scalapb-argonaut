package scalapb_argonaut

import utest._
import com.google.protobuf.struct._
import jsontest.test3.StructTest

object StructFormatSpec extends TestSuite with JavaAssertions {
  override val tests = Tests {
    "Empty value should be serialized to null" - {
      assert(JsonFormat.toJsonString(Value()) == "null")
    }
    "NullValue should be serialized and parsed from JSON correctly" - {
      assert(JsonFormat.fromJsonString[StructTest]("""{"nv": null}""") == StructTest())
      assert(JsonFormat.fromJsonString[StructTest]("""{"nv": "NULL_VALUE"}""") == StructTest())
      assert(JsonFormat.fromJsonString[StructTest]("""{"nv": 0}""") == StructTest())
      assert(
        JsonFormat.fromJsonString[StructTest]("""{"repNv": [null, 0, null]}""") ==
          StructTest(repNv = Seq(NullValue.NULL_VALUE, NullValue.NULL_VALUE, NullValue.NULL_VALUE))
      )
    }
  }
}
