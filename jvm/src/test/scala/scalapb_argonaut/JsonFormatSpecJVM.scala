package scalapb_argonaut

import utest._
import jsontest.test._
import com.google.protobuf.util.{JsonFormat => JavaJsonFormat}
import com.google.protobuf.any.{Any => PBAny}
import com.google.protobuf.util.JsonFormat.{TypeRegistry => JavaTypeRegistry}
import scalapb_json._

object JsonFormatSpecJVM extends TestSuite {

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

  override val tests = Tests {

    "fromJsonString should read json produced by Java" - {
      val javaJson = JavaJsonFormat.printer().print(MyTest.toJavaProto(TestProto))
      assert(JsonFormat.fromJsonString[MyTest](javaJson) == TestProto)
    }

    "Java parser should read json strings produced by us" - {
      val b = jsontest.Test.MyTest.newBuilder
      JavaJsonFormat.parser().merge(JsonFormat.toJsonString(TestProto), b)
      assert(TestProto == MyTest.fromJavaProto(b.build))
    }

    val anyEnabledJavaTypeRegistry =
      JavaTypeRegistry.newBuilder().add(TestProto.companion.javaDescriptor).build()
    val anyEnabledJavaPrinter =
      JavaJsonFormat.printer().usingTypeRegistry(anyEnabledJavaTypeRegistry)
    val anyEnabledTypeRegistry = TypeRegistry.empty.addMessageByCompanion(TestProto.companion)
    val anyEnabledParser = new Parser(typeRegistry = anyEnabledTypeRegistry)

    "Any should parse JSON produced by Java for a packed TestProto" - {
      val javaAny = com.google.protobuf.Any.pack(MyTest.toJavaProto(TestProto))
      val javaJson = anyEnabledJavaPrinter.print(javaAny)
      assert(anyEnabledParser.fromJsonString[PBAny](javaJson).unpack[MyTest] == TestProto)
    }
  }

}
