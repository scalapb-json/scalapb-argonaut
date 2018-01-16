package scalapb_argonaut

import argonaut._
import argonaut.JsonParser.parse
import utest._
import jsontest.test._
import jsontest.test3._
import com.google.protobuf.any.{Any => PBAny}
import jsontest.custom_collection.{Guitar, Studio}
import scalapb_json._
import EitherOps._

object JsonFormatSpec extends TestSuite {

  val TestProto = MyTest().update(
    _.hello := "Foo",
    _.foobar := 37,
    _.primitiveSequence := Seq("a", "b", "c"),
    _.repMessage := Seq(MyTest(), MyTest(hello = Some("h11"))),
    _.optMessage := MyTest().update(_.foobar := 39),
    _.stringToInt32 := Map("foo" -> 14, "bar" -> 19),
    _.intToMytest := Map(14 -> MyTest(), 35 -> MyTest(hello = Some("boo"))),
    _.repEnum := Seq(MyEnum.V1, MyEnum.V2, MyEnum.UNKNOWN),
    _.optEnum := MyEnum.V2,
    _.intToEnum := Map(32 -> MyEnum.V1, 35 -> MyEnum.V2),
    _.stringToBool := Map("ff" -> false, "tt" -> true),
    _.boolToString := Map(false -> "ff", true -> "tt"),
    _.optBool := false
  )

  val TestJson =
    """{
      |  "hello": "Foo",
      |  "foobar": 37,
      |  "primitiveSequence": ["a", "b", "c"],
      |  "repMessage": [{}, {"hello": "h11"}],
      |  "optMessage": {"foobar": 39},
      |  "stringToInt32": {"foo": 14, "bar": 19},
      |  "intToMytest": {"14": {}, "35": {"hello": "boo"}},
      |  "repEnum": ["V1", "V2", "UNKNOWN"],
      |  "optEnum": "V2",
      |  "intToEnum": {"32": "V1", "35": "V2"},
      |  "stringToBool": {"ff": false, "tt": true},
      |  "boolToString": {"false": "ff", "true": "tt"},
      |  "optBool": false
      |}
      |""".stripMargin

  val TestJsonWithType =
    """{
      |  "@type": "type.googleapis.com/jsontest.MyTest",
      |  "hello": "Foo",
      |  "foobar": 37,
      |  "primitiveSequence": ["a", "b", "c"],
      |  "repMessage": [{}, {"hello": "h11"}],
      |  "optMessage": {"foobar": 39},
      |  "stringToInt32": {"foo": 14, "bar": 19},
      |  "intToMytest": {"14": {}, "35": {"hello": "boo"}},
      |  "repEnum": ["V1", "V2", "UNKNOWN"],
      |  "optEnum": "V2",
      |  "intToEnum": {"32": "V1", "35": "V2"},
      |  "stringToBool": {"ff": false, "tt": true},
      |  "boolToString": {"false": "ff", "true": "tt"},
      |  "optBool": false
      |}
      |""".stripMargin

  val DefaultTestJson =
    """{
      |  "hello": "",
      |  "foobar": 0,
      |  "bazinga": 0,
      |  "primitiveSequence": [],
      |  "repMessage": [],
      |  "stringToInt32": {},
      |  "intToMytest": {},
      |  "repEnum": [],
      |  "optEnum": "UNKNOWN",
      |  "intToEnum": {},
      |  "boolToString": {},
      |  "stringToBool": {},
      |  "optBool": false
      |}""".stripMargin

  val PreservedTestJson =
    """{
      |  "hello": "Foo",
      |  "foobar": 37,
      |  "primitive_sequence": ["a", "b", "c"],
      |  "rep_message": [{}, {"hello": "h11"}],
      |  "opt_message": {"foobar": 39},
      |  "string_to_int32": {"foo": 14, "bar": 19},
      |  "int_to_mytest": {"14": {}, "35": {"hello": "boo"}},
      |  "rep_enum": ["V1", "V2", "UNKNOWN"],
      |  "opt_enum": "V2",
      |  "int_to_enum": {"32": "V1", "35": "V2"},
      |  "string_to_bool": {"ff": false, "tt": true},
      |  "bool_to_string": {"false": "ff", "true": "tt"},
      |  "opt_bool": false
      |}
      |""".stripMargin

  override val tests = Tests {
    "Empty object should give empty json" - {
      assert(JsonFormat.toJson(MyTest()) == Json.obj())
    }
  
    "Empty object should give empty json for MyTest3" - {
      assert(JsonFormat.toJson(MyTest3()) == Json.obj())
    }
  
    "Zero maps should give correct json" - {
      assert(JsonFormat.toJson(
        MyTest(
          stringToInt32 = Map("" -> 17),
          intToMytest = Map(0 -> MyTest()),
          fixed64ToBytes = Map(0L -> com.google.protobuf.ByteString.copyFromUtf8("foobar")))) == 
        parse("""|{
                   |  "stringToInt32": {"": 17},
                   |  "intToMytest": {"0": {}},
                   |  "fixed64ToBytes": {"0": "Zm9vYmFy"}
                   |}""".stripMargin).getOrError)
    }
  
    "Zero maps should give correct json for MyTest3" - {
      assert(JsonFormat.toJson(
        MyTest3(
          stringToInt32 = Map("" -> 17),
          intToMytest = Map(0 -> MyTest()),
          fixed64ToBytes = Map(0L -> com.google.protobuf.ByteString.copyFromUtf8("foobar")))) == 
        parse("""|{
                   |  "stringToInt32": {"": 17},
                   |  "intToMytest": {"0": {}},
                   |  "fixed64ToBytes": {"0": "Zm9vYmFy"}
                   |}""".stripMargin).getOrError)
    }
  
    "Set treat should give correct json" - {
      assert(JsonFormat.toJson(MyTest(trickOrTreat = MyTest.TrickOrTreat.Treat(MyTest()))) == 
        parse("""{"treat": {}}""").getOrError)
    }
  
    "Parse treat should give correct proto with proto2" - {
      assert(JsonFormat.fromJsonString[MyTest]("""{"treat": {"hello": "x"}}""") == 
        MyTest(trickOrTreat = MyTest.TrickOrTreat.Treat(MyTest(hello = Some("x")))))
      assert(JsonFormat.fromJsonString[MyTest]("""{"treat": {}}""") == 
        MyTest(trickOrTreat = MyTest.TrickOrTreat.Treat(MyTest())))
    }
  
    "Parse treat should give correct proto with proto3" - {
      assert(JsonFormat.fromJsonString[MyTest3]("""{"treat": {"s": "x"}}""") == 
        MyTest3(trickOrTreat = MyTest3.TrickOrTreat.Treat(MyTest3(s = "x"))))
      assert(JsonFormat.fromJsonString[MyTest3]("""{"treat": {}}""") == 
        MyTest3(trickOrTreat = MyTest3.TrickOrTreat.Treat(MyTest3())))
    }
  
    "parsing one offs should work correctly for issue 315" - {
      assert(JsonFormat.fromJsonString[jsontest.issue315.Msg]("""
      {
            "baz" : "1",
            "foo" : {
              "cols" : "1"
            }
      }""") == 
        jsontest.issue315.Msg(
          baz = "1",
          someUnion = jsontest.issue315.Msg.SomeUnion.Foo(jsontest.issue315.Foo(cols = "1"))))
    }
  
    "parsing null should give default value" - {
      assert(JsonFormat.fromJsonString[jsontest.test.MyTest]("""
      {
            "optMessage" : null,
            "optBool": null,
            "optEnum": null,
            "repEnum": null
      }""") == jsontest.test.MyTest())
    }
  
    "TestProto should be TestJson when converted to Proto" - {
      assert(JsonFormat.toJson(TestProto) == parse(TestJson).getOrError)
    }
  
    "TestJson should be TestProto when parsed from json" - {
      assert(JsonFormat.fromJsonString[MyTest](TestJson) == TestProto)
    }
  
    "Empty object should give full json if including default values" - {
      assert(new Printer(includingDefaultValueFields = true).toJson(MyTest()) == 
        parse("""{
            |  "hello": "",
            |  "foobar": 0,
            |  "bazinga": "0",
            |  "primitiveSequence": [],
            |  "repMessage": [],
            |  "stringToInt32": {},
            |  "intToMytest": {},
            |  "repEnum": [],
            |  "optEnum": "UNKNOWN",
            |  "intToEnum": {},
            |  "boolToString": {},
            |  "stringToBool": {},
            |  "optBs": "",
            |  "optBool": false,
            |  "trick": 0,
            |  "fixed64ToBytes": {}
            |}""".stripMargin).getOrError
      )
    }
  
    "Empty object should with preserve field names should work" - {
      assert(new Printer(includingDefaultValueFields = true, preservingProtoFieldNames = true)
        .toJson(MyTest()) == 
        parse("""{
            |  "hello": "",
            |  "foobar": 0,
            |  "bazinga": "0",
            |  "primitive_sequence": [],
            |  "rep_message": [],
            |  "string_to_int32": {},
            |  "int_to_mytest": {},
            |  "rep_enum": [],
            |  "opt_enum": "UNKNOWN",
            |  "int_to_enum": {},
            |  "bool_to_string": {},
            |  "string_to_bool": {},
            |  "opt_bs": "",
            |  "opt_bool": false,
            |  "trick": 0,
            |  "fixed64_to_bytes": {}
            |}""".stripMargin).getOrError
      )
    }
  
    "TestProto should format int64 as JSON string" - {
      assert(new Printer().print(MyTest(bazinga = Some(642))) == """{"bazinga":"642"}""")
    }
  
    "TestProto should format int64 as JSON number" - {
      assert(new Printer(formattingLongAsNumber = true).print(MyTest(bazinga = Some(642))) == 
        """{"bazinga":642}""")
    }
  
    "TestProto should parse numbers formatted as JSON string" - {
      val parser = new Parser()
      def validateAccepts(json: String, expected: IntFields) = {
        assert(parser.fromJsonString[IntFields](json) == expected)
      }
      def validateRejects(json: String) = try {
        parser.fromJsonString[IntFields](json)
        sys.error("fail")
      } catch {
        case _: JsonFormatException =>
      }
  
      // int32
      validateAccepts("""{"int":"642"}""", IntFields(int = Some(642)))
      validateAccepts("""{"int":"-1"}""", IntFields(int = Some(-1)))
      validateAccepts(s"""{"int":"${Integer.MAX_VALUE}"}""", IntFields(int = Some(Integer.MAX_VALUE)))
      validateAccepts(s"""{"int":"${Integer.MIN_VALUE}"}""", IntFields(int = Some(Integer.MIN_VALUE)))
      validateRejects(s"""{"int":"${Integer.MAX_VALUE.toLong + 1}"}""")
      validateRejects(s"""{"int":"${Integer.MIN_VALUE.toLong - 1}"}""")
  
      // int64
      validateAccepts("""{"long":"642"}""", IntFields(long = Some(642L)))
      validateAccepts("""{"long":"-1"}""", IntFields(long = Some(-1L)))
      validateAccepts(s"""{"long":"${Long.MaxValue}"}""", IntFields(long = Some(Long.MaxValue)))
      validateAccepts(s"""{"long":"${Long.MinValue}"}""", IntFields(long = Some(Long.MinValue)))
      validateRejects(s"""{"long":"${BigInt(Long.MaxValue) + 1}"}""")
      validateRejects(s"""{"long":"${BigInt(Long.MinValue) - 1}"}""")
  
      // uint32
      val uint32max: Long = (1L << 32) - 1
      validateAccepts(s"""{"uint":"$uint32max"}""", IntFields(uint = Some(uint32max.toInt)))
      validateRejects(s"""{"uint":"${uint32max + 1}"}""")
      validateRejects("""{"uint":"-1"}""")
  
      // uint64
      val uint64max: BigInt = (BigInt(1) << 64) - 1
      validateAccepts(s"""{"ulong":"$uint64max"}""", IntFields(ulong = Some(uint64max.toLong)))
      validateRejects(s"""{"ulong":"${uint64max + 1}"}""")
      validateRejects("""{"ulong":"-1"}""")
  
      // sint32
      validateAccepts(
        s"""{"sint":"${Integer.MAX_VALUE}"}""",
        IntFields(sint = Some(Integer.MAX_VALUE)))
      validateAccepts(
        s"""{"sint":"${Integer.MIN_VALUE}"}""",
        IntFields(sint = Some(Integer.MIN_VALUE)))
      validateRejects(s"""{"sint":"${Integer.MAX_VALUE.toLong + 1}"}""")
      validateRejects(s"""{"sint":"${Integer.MIN_VALUE.toLong - 1}"}""")
  
      // sint64
      validateAccepts(s"""{"slong":"${Long.MaxValue}"}""", IntFields(slong = Some(Long.MaxValue)))
      validateAccepts(s"""{"slong":"${Long.MinValue}"}""", IntFields(slong = Some(Long.MinValue)))
      validateRejects(s"""{"slong":"${BigInt(Long.MaxValue) + 1}"}""")
      validateRejects(s"""{"slong":"${BigInt(Long.MinValue) - 1}"}""")
  
      // fixed32
      validateAccepts(s"""{"fixint":"$uint32max"}""", IntFields(fixint = Some(uint32max.toInt)))
      validateRejects(s"""{"fixint":"${uint32max + 1}"}""")
      validateRejects("""{"fixint":"-1"}""")
  
      // fixed64
      validateAccepts(s"""{"fixlong":"$uint64max"}""", IntFields(fixlong = Some(uint64max.toLong)))
      validateRejects(s"""{"fixlong":"${uint64max + 1}"}""")
      validateRejects("""{"fixlong":"-1"}""")
  
    }
  
    "TestProto should produce valid JSON output for unsigned integers" - {
      val uint32max: Long = (1L << 32) - 1
      assert(JsonFormat.toJson(IntFields(uint = Some(uint32max.toInt))) == 
        parse(s"""{"uint":$uint32max}""").getOrError)
      assert(JsonFormat.toJson(IntFields(uint = Some(1))) == parse(s"""{"uint":1}""").getOrError)
      assert(JsonFormat.toJson(IntFields(fixint = Some(uint32max.toInt))) == 
        parse(s"""{"fixint":$uint32max}""").getOrError)
      assert(JsonFormat.toJson(IntFields(fixint = Some(1))) == parse(s"""{"fixint":1}""").getOrError)
      val uint64max: BigInt = (BigInt(1) << 64) - 1
      assert(JsonFormat.toJson(IntFields(ulong = Some(uint64max.toLong))) == 
        parse(s"""{"ulong":"$uint64max"}""").getOrError)
      assert(JsonFormat.toJson(IntFields(ulong = Some(1))) == parse(s"""{"ulong":"1"}""").getOrError)
      assert(JsonFormat.toJson(IntFields(fixlong = Some(uint64max.toLong))) == 
        parse(s"""{"fixlong":"$uint64max"}""").getOrError)
      assert(JsonFormat.toJson(IntFields(fixlong = Some(1))) == parse(s"""{"fixlong":"1"}""").getOrError)
    }
  
    "TestProto should parse an enum formatted as number" - {
      assert(new Parser().fromJsonString[MyTest]("""{"optEnum":1}""") == 
        MyTest(optEnum = Some(MyEnum.V1)))
      assert(new Parser().fromJsonString[MyTest]("""{"optEnum":2}""") == 
        MyTest(optEnum = Some(MyEnum.V2)))
    }
  
    "PreservedTestJson should be TestProto when parsed from json" - {
      assert(new Parser(preservingProtoFieldNames = true).fromJsonString[MyTest](PreservedTestJson) == 
        TestProto)
    }
  
    "DoubleFloatProto should parse NaNs" - {
      val i = s"""{
        "d": "NaN",
        "f": "NaN"
      }"""
      val out = JsonFormat.fromJsonString[DoubleFloat](i)
      assert(out.d.map(_.isNaN) == Some(true))
      assert(out.f.map(_.isNaN) == Some(true))
      assert(JsonFormat.toJson(out).obj.flatMap(_.apply("d")) == Some(Json.jString(Double.NaN.toString)))
      assert(JsonFormat.toJson(out).obj.flatMap(_.apply("f")) == Some(Json.jString(Double.NaN.toString)))
    }
  
    "DoubleFloatProto should parse Infinity" - {
      val i = s"""{
        "d": "Infinity",
        "f": "Infinity"
      }"""
      val out = JsonFormat.fromJsonString[DoubleFloat](i)
      assert(out.d.map(_.isPosInfinity) == Some(true))
      assert(out.f.map(_.isPosInfinity) == Some(true))
      assert(JsonFormat.toJson(out).obj.flatMap(_.apply("d")) == Some(Json.jString(Double.PositiveInfinity.toString)))
      assert(JsonFormat.toJson(out).obj.flatMap(_.apply("f")) == Some(Json.jString(Double.PositiveInfinity.toString)))
    }
  
    "DoubleFloatProto should parse -Infinity" - {
      val i = s"""{
        "d": "-Infinity",
        "f": "-Infinity"
      }"""
      val out = JsonFormat.fromJsonString[DoubleFloat](i)
      assert(out.d.map(_.isNegInfinity) == Some(true))
      assert(out.f.map(_.isNegInfinity) == Some(true))
      assert(JsonFormat.toJson(out).obj.flatMap(_.apply("d")) == Some(Json.jString(Double.NegativeInfinity.toString)))
      assert(JsonFormat.toJson(out).obj.flatMap(_.apply("f")) == Some(Json.jString(Double.NegativeInfinity.toString)))
    }
  
    val anyEnabledTypeRegistry = TypeRegistry.empty.addMessageByCompanion(TestProto.companion)
    val anyEnabledParser = new Parser(typeRegistry = anyEnabledTypeRegistry)
    val anyEnabledPrinter = new Printer(typeRegistry = anyEnabledTypeRegistry)

    "TestProto packed as any should give TestJsonWithType after JSON serialization" - {
      val any = PBAny.pack(TestProto)
  
      assert(anyEnabledPrinter.toJson(any) == parse(TestJsonWithType).getOrError)
    }
  
    "TestJsonWithType should be TestProto packed as any when parsed from JSON" - {
      val out = anyEnabledParser.fromJson[PBAny](parse(TestJsonWithType).getOrError)
      assert(out.unpack[MyTest] == TestProto)
    }
  
    "toJsonString should generate correct JSON for messages with custom collection type" - {
      val studio = Studio().addGuitars(Guitar(numberOfStrings = 12))
      val expectedStudioJsonString = """{"guitars":[{"numberOfStrings":12}]}"""
      val studioJsonString = JsonFormat.toJsonString(studio)
      assert(studioJsonString == expectedStudioJsonString)
    }
  
    "fromJsonString should parse JSON correctly to message with custom collection type" - {
      val expectedStudio = Studio().addGuitars(Guitar(numberOfStrings = 12))
      val studioJsonString = """{"guitars":[{"numberOfStrings":12}]}"""
      val studio = JsonFormat.fromJsonString[Studio](studioJsonString)
      assert(studio == expectedStudio)
    }
  }
}
