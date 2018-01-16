package scalapb_argonaut

import com.google.protobuf.any.{Any => PBAny}
import jsontest.anytests.{AnyTest, ManyAnyTest}
import utest._
import argonaut.JsonParser.parse
import scalapb_json._
import EitherOps._

object AnyFormatSpec extends TestSuite with JavaAssertions {
  val RawExample = AnyTest("test")

  val RawJson = parse(s"""{"field":"test"}""").getOrError

  val AnyExample = PBAny.pack(RawExample)

  val AnyJson = parse(s"""{"@type":"type.googleapis.com/jsontest.AnyTest","field":"test"}""").getOrError

  val CustomPrefixAny = PBAny.pack(RawExample, "example.com/")

  val CustomPrefixJson = parse(s"""{"@type":"example.com/jsontest.AnyTest","field":"test"}""").getOrError

  val ManyExample = ManyAnyTest(
    Seq(
      PBAny.pack(AnyTest("1")),
      PBAny.pack(AnyTest("2"))
    ))

  val ManyPackedJson = parse("""
      |{
      |  "@type": "type.googleapis.com/jsontest.ManyAnyTest",
      |  "fields": [
      |    {"@type": "type.googleapis.com/jsontest.AnyTest", "field": "1"},
      |    {"@type": "type.googleapis.com/jsontest.AnyTest", "field": "2"}
      |  ]
      |}
    """.stripMargin).getOrError

  override def registeredCompanions = Seq(AnyTest, ManyAnyTest)

  // For clarity
  def UnregisteredPrinter = JsonFormat.printer

  def UnregisteredParser = JsonFormat.parser

  val tests = Tests {
    "Any should fail to serialize if its respective companion is not registered" - {
      try {
        UnregisteredPrinter.toJson(AnyExample)
        sys.error("fail")
      } catch {
        case _: IllegalStateException =>
      }
    }

    "Any should fail to deserialize if its respective companion is not registered" - {
      try {
        UnregisteredParser.fromJson[PBAny](AnyJson)
        sys.error("fail")
      } catch {
        case _: JsonFormatException =>
      }
    }

    "Any should serialize correctly if its respective companion is registered" - {
      assert(ScalaJsonPrinter.toJson(AnyExample) == AnyJson)
    }

    "Any should fail to serialize with a custom URL prefix if specified" - {
      try {
        ScalaJsonPrinter.toJson(CustomPrefixAny)
        sys.error("fail")
      } catch {
        case _: IllegalStateException =>
      }
    }

    "Any should fail to deserialize for a non-Google-prefixed type URL" - {
      try {
        ScalaJsonParser.fromJson[PBAny](CustomPrefixJson)
        sys.error("fail")
      } catch {
        case _: JsonFormatException =>
      }
    }

    "Any should deserialize correctly if its respective companion is registered" - {
      assert(ScalaJsonParser.fromJson[PBAny](AnyJson) == AnyExample)
    }

    "Any should resolve printers recursively" - {
      val packed = PBAny.pack(ManyExample)
      assert(ScalaJsonPrinter.toJson(packed) == ManyPackedJson)
    }

    "Any should resolve parsers recursively" - {
      assert(ScalaJsonParser.fromJson[PBAny](ManyPackedJson).unpack[ManyAnyTest] == ManyExample)
    }
  }
}
