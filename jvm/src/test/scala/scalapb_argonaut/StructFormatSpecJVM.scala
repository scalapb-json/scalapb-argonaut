package scalapb_argonaut

import utest._
import com.google.protobuf.struct._
import jsontest.test3.StructTest

object StructFormatSpecJVM extends TestSuite with JavaAssertions {
  val ListValueExample = ListValue(
    values = Seq(
      Value(Value.Kind.NumberValue(-245.0)),
      Value(Value.Kind.BoolValue(true)),
      Value(Value.Kind.StringValue("Boom"))
    )
  )

  val StructExample = Struct(
    fields = Map(
      "f1" -> Value(Value.Kind.StringValue("Boo")),
      "f2" -> Value(Value.Kind.ListValue(ListValueExample)),
      "f3" -> Value(
        Value.Kind.StructValue(Struct(fields = Map("f4" -> Value(Value.Kind.StringValue("f5")))))
      )
    )
  )

  val StructExample2 = Struct(
    fields = Map(
      "f1" -> Value(Value.Kind.StringValue("Boo")),
      "f2" -> Value(Value.Kind.StructValue(StructExample)),
      "f3" -> Value(Value.Kind.NullValue(NullValue.NULL_VALUE))
    )
  )

  override val tests = Tests {
    "Empty value should be serialized to null" - {
      assert(JavaJsonPrinter.print(com.google.protobuf.Value.newBuilder().build()) == "null")
    }

    "Value should be serialized the same as in Java (and parsed back to original)" - {
      assertJsonIsSameAsJava(Value(kind = Value.Kind.NumberValue(1.0)))
      assertJsonIsSameAsJava(Value(kind = Value.Kind.NumberValue(-25)))
      assertJsonIsSameAsJava(
        Value(kind = Value.Kind.NumberValue(Double.PositiveInfinity)),
        checkRoundtrip = false
      )
      assertJsonIsSameAsJava(
        Value(kind = Value.Kind.NumberValue(Double.NegativeInfinity)),
        checkRoundtrip = false
      )
      assertJsonIsSameAsJava(
        Value(kind = Value.Kind.NumberValue(Double.NaN)),
        checkRoundtrip = false
      )
      assertJsonIsSameAsJava(Value(kind = Value.Kind.StringValue("boo")))
      assertJsonIsSameAsJava(Value(kind = Value.Kind.BoolValue(true)))
      assertJsonIsSameAsJava(Value(kind = Value.Kind.BoolValue(false)))
      assertJsonIsSameAsJava(
        Value(kind = Value.Kind.NullValue(com.google.protobuf.struct.NullValue.NULL_VALUE))
      )
      assertJsonIsSameAsJava(Value(kind = Value.Kind.StructValue(value = StructExample)))

      assertJsonIsSameAsJava(
        Value(
          kind = Value.Kind.ListValue(
            com.google.protobuf.struct.ListValue(
              values = Seq(
                Value(Value.Kind.NumberValue(-17.0)),
                Value(Value.Kind.StringValue("Boo")),
                Value(Value.Kind.StructValue(StructExample2)),
                Value(Value.Kind.BoolValue(false)),
                Value(Value.Kind.ListValue(ListValueExample))
              )
            )
          )
        )
      )
    }

    "Struct should be serialized the same as in Java (and parsed back to original)" - {
      assertJsonIsSameAsJava(Struct())
      assertJsonIsSameAsJava(StructExample)
      assertJsonIsSameAsJava(StructExample2)
    }

    "ListValue should be serialized the same as in Java (and parsed back to original)" - {
      assertJsonIsSameAsJava(ListValue())
      assertJsonIsSameAsJava(ListValueExample)
    }

    "NullValue should be serialized and parsed from JSON correctly" - {
      assert(javaParse("""{"nv": 0}""", jsontest.Test3.StructTest.newBuilder).toString == "")
      assert(
        javaParse("""{"repNv": [0,0.0,0]}""", jsontest.Test3.StructTest.newBuilder).toString ==
          "rep_nv: NULL_VALUE\n" * 3
      )
      assertJsonIsSameAsJava(StructTest())
      assertJsonIsSameAsJava(StructTest(nv = NullValue.NULL_VALUE))
      assertJsonIsSameAsJava(StructTest(repNv = Seq(NullValue.NULL_VALUE, NullValue.NULL_VALUE)))
    }
  }
}
