package scalapb_argonaut

import EitherOps._
import argonaut.JsonParser.parse
import com.google.protobuf.util.JsonFormat.{printer => ProtobufJavaPrinter}
import jsontest.oneof.OneOf._
import jsontest.oneof.{OneOf, OneOfMessage}
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
      examples.foreach {
        case (message: OneOf, json: String) =>
          assert(
            new Printer(includingDefaultValueFields = false)
              .toJson(message) == parse(json).getOrError)
          assert(
            new Printer(includingDefaultValueFields = false).toJson(message) ==
              parse(
                ProtobufJavaPrinter().print(toJavaProto(message))
              ).getOrError)

          assert(
            new Printer(includingDefaultValueFields = true)
              .toJson(message) == parse(json).getOrError)
          assert(
            new Printer(includingDefaultValueFields = true).toJson(message) ==
              parse(
                ProtobufJavaPrinter().includingDefaultValueFields().print(toJavaProto(message))
              ).getOrError)
      }
    }
  }

}
