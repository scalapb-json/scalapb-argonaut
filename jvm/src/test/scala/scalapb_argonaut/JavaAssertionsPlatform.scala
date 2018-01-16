package scalapb_argonaut

import com.google.protobuf.util.JsonFormat.{TypeRegistry => JavaTypeRegistry}
import scalapb.{GeneratedMessage, GeneratedMessageCompanion, JavaProtoSupport, Message}
import utest._

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
    checkRoundtrip: Boolean = true)(implicit cmp: GeneratedMessageCompanion[T]) = {
    val scalaJson = ScalaJsonPrinter.print(v)
    val javaJson = JavaJsonPrinter.print(
      cmp.asInstanceOf[JavaProtoSupport[T, com.google.protobuf.GeneratedMessageV3]].toJavaProto(v))

    import argonaut.JsonParser.parse
    assert(parse(scalaJson).isRight)
    assert(parse(scalaJson) == parse(javaJson))
    if (checkRoundtrip) {
      assert(ScalaJsonParser.fromJsonString[T](scalaJson) == v)
    }
  }

  def javaParse[T <: com.google.protobuf.GeneratedMessageV3.Builder[T]](
    json: String,
    b: com.google.protobuf.GeneratedMessageV3.Builder[T]) = {
    JavaJsonParser.merge(json, b)
    b.build()
  }
}
