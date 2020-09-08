package scalapb_argonaut

import EitherOps._
import argonaut.JsonParser.parse
import com.google.protobuf.util.JsonFormat.{printer => ProtobufJavaPrinter}
import jsontest.oneof.OneOf._
import jsontest.oneof.Pair.ValueByType._
import jsontest.oneof.{Dictionary, OneOf, OneOfMessage, Pair}
import utest._

object OneOfSpec extends TestSuite {
  private[this] val examples = List(
    (OneOf.defaultInstance, "{}"),
    (OneOf(Field.Empty), "{}"),
    (OneOf(Field.Primitive("")), """{"primitive":""}"""),
    (OneOf(Field.Primitive("test")), """{"primitive":"test"}"""),
    (OneOf(Field.Wrapper("")), """{"wrapper":""}"""),
    (OneOf(Field.Wrapper("test")), """{"wrapper":"test"}"""),
    (OneOf(Field.Message(OneOfMessage())), """{"message":{}}"""),
    (OneOf(Field.Message(OneOfMessage(Some("test")))), """{"message":{"field":"test"}}""")
  )

  override val tests = Tests {
    "oneof" - {
      examples.foreach { case (message: OneOf, json: String) =>
        assert(
          new Printer().toJson(message) == parse(json).getOrError
        )
        assert(
          new Printer().toJson(message) ==
            parse(
              ProtobufJavaPrinter().print(toJavaProto(message))
            ).getOrError
        )

        assert(
          new Printer().includingDefaultValueFields.toJson(message) == parse(json).getOrError
        )
        assert(
          new Printer().includingDefaultValueFields.toJson(message) ==
            parse(
              ProtobufJavaPrinter().includingDefaultValueFields().print(toJavaProto(message))
            ).getOrError
        )
      }
    }

    "dictionary test should preserve zero values in one of" - {
      val message = Dictionary(Seq(Pair("myKey", Uint32Value(0))))

      assert(
        new Printer().toJson(message) ==
          parse("""{"pairs":[{"key": "myKey", "uint32Value": 0}]}""").getOrError
      )

      assert(
        new Printer().includingDefaultValueFields.toJson(message) ==
          parse("""{"pairs":[{"key": "myKey", "uint32Value": 0}]}""").getOrError
      )
    }
  }
}
