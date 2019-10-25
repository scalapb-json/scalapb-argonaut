package scalapb_argonaut

import com.google.protobuf.{GeneratedMessageV3, InvalidProtocolBufferException}
import com.google.protobuf.util.JsonFormat.{TypeRegistry => JavaTypeRegistry}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, JavaProtoSupport, Message}
import utest._
import scalapb_json.JsonFormatException
import JsonFormatSpecBase.assertThrows
import scala.reflect.ClassTag

trait JavaAssertionsPlatform {
  self: TestSuite with JavaAssertions =>

  def registeredCompanions: Seq[GeneratedMessageCompanion[_]]

  val JavaJsonTypeRegistry =
    registeredCompanions.foldLeft(JavaTypeRegistry.newBuilder())(_ add _.javaDescriptor).build()
  val JavaJsonPrinter =
    com.google.protobuf.util.JsonFormat.printer().usingTypeRegistry(JavaJsonTypeRegistry)
  val JavaJsonParser = com.google.protobuf.util.JsonFormat.parser()

  def assertJsonIsSameAsJava[T <: GeneratedMessage with Message[T]](
    v: T,
    checkRoundtrip: Boolean = true
  )(implicit cmp: GeneratedMessageCompanion[T]) = {
    val scalaJson = ScalaJsonPrinter.print(v)
    val javaJson = JavaJsonPrinter.print(
      cmp.asInstanceOf[JavaProtoSupport[T, com.google.protobuf.GeneratedMessageV3]].toJavaProto(v)
    )

    import argonaut.JsonParser.parse
    assert(parse(scalaJson).isRight)
    assert(parse(scalaJson) == parse(javaJson))
    if (checkRoundtrip) {
      assert(ScalaJsonParser.fromJsonString[T](scalaJson) == v)
    }
  }

  def javaParse[T <: com.google.protobuf.GeneratedMessageV3.Builder[T]](
    json: String,
    b: com.google.protobuf.GeneratedMessageV3.Builder[T]
  ) = {
    JavaJsonParser.merge(json, b)
    b.build()
  }

  def assertParse[T <: scalapb.GeneratedMessage with scalapb.Message[T], J <: GeneratedMessageV3](
    json: String,
    expected: T
  )(
    implicit cmp: GeneratedMessageCompanion[T] with JavaProtoSupport[T, J],
    parserContext: ParserContext
  ) = {
    val parsedJava: J = {
      val builder = cmp.toJavaProto(cmp.defaultInstance).newBuilderForType()
      parserContext.javaParser.merge(json, builder)
      builder.build().asInstanceOf[J]
    }

    val parsedScala = parserContext.scalaParser.fromJsonString[T](json)(cmp)
    assert(parsedScala == expected)
    assert(cmp.fromJavaProto(parsedJava) == expected)
  }

  def assertFails[T <: scalapb.GeneratedMessage with scalapb.Message[T], J <: GeneratedMessageV3](
    json: String,
    cmp: GeneratedMessageCompanion[T] with JavaProtoSupport[T, J]
  )(implicit parserContext: ParserContext) = {
    val builder = cmp.toJavaProto(cmp.defaultInstance).newBuilderForType()
    assertThrows[InvalidProtocolBufferException] {
      parserContext.javaParser.merge(json, builder)
    }
    assertThrows[JsonFormatException] {
      parserContext.scalaParser.fromJsonString[T](json)(cmp)
    }
  }
}
