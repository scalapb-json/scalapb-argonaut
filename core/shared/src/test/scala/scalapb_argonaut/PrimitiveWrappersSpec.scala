package scalapb_argonaut

import com.google.protobuf.ByteString
import jsontest.test3._
import argonaut._
import utest._

object PrimitiveWrappersSpec extends TestSuite {

  private[this] def render[A](a: A)(implicit A: EncodeJson[A]): Json =
    A.apply(a)

  override val tests = Tests {

    "Empty object should give empty json for Wrapper" - {
      assert(JsonFormat.toJson(Wrapper()) == render(Map.empty[String, Json]))
    }

    "primitive values should serialize properly" - {
      assert(
        JsonFormat.toJson(Wrapper(wBool = Some(false))) ==
          render(Map("wBool" -> Json.jBool(false)))
      )
      assert(
        JsonFormat.toJson(Wrapper(wBool = Some(true))) == render(Map("wBool" -> Json.jBool(true)))
      )
      assert(
        JsonFormat.toJson(Wrapper(wDouble = Some(3.1))) == render(
          Map("wDouble" -> Json.jNumber(3.1))
        )
      )
      assert(
        JsonFormat.toJson(Wrapper(wFloat = Some(3.0f))) == render(
          Map("wFloat" -> Json.jNumber(3.0))
        )
      )
      assert(
        JsonFormat.toJson(Wrapper(wInt32 = Some(35544))) ==
          render(Map("wInt32" -> Json.jNumber(35544)))
      )
      assert(
        JsonFormat.toJson(Wrapper(wInt32 = Some(0))) == render(Map("wInt32" -> Json.jNumber(0)))
      )
      assert(
        JsonFormat.toJson(Wrapper(wInt64 = Some(125))) == render(
          Map("wInt64" -> Json.jString("125"))
        )
      )
      assert(
        JsonFormat.toJson(Wrapper(wUint32 = Some(125))) == render(
          Map("wUint32" -> Json.jNumber(125))
        )
      )
      assert(
        JsonFormat.toJson(Wrapper(wUint64 = Some(125))) ==
          render(Map("wUint64" -> Json.jString("125")))
      )
      assert(
        JsonFormat.toJson(Wrapper(wString = Some("bar"))) ==
          render(Map("wString" -> Json.jString("bar")))
      )
      assert(
        JsonFormat.toJson(Wrapper(wString = Some(""))) == render(Map("wString" -> Json.jString("")))
      )
      assert(
        JsonFormat.toJson(Wrapper(wBytes = Some(ByteString.copyFrom(Array[Byte](3, 5, 4))))) ==
          render(Map("wBytes" -> Json.jString("AwUE")))
      )
      assert(
        JsonFormat.toJson(Wrapper(wBytes = Some(ByteString.EMPTY))) ==
          render(Map("wBytes" -> Json.jString("")))
      )
    }

    "primitive values should parse properly" - {
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wBool" -> Json.jBool(false)))) ==
          Wrapper(wBool = Some(false))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wBool" -> Json.jBool(true)))) ==
          Wrapper(wBool = Some(true))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wDouble" -> Json.jNumber(3.1)))) ==
          Wrapper(wDouble = Some(3.1))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wDouble" -> Json.jString("3.1")))) ==
          Wrapper(wDouble = Some(3.1))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wFloat" -> Json.jNumber(3.0)))) ==
          Wrapper(wFloat = Some(3.0f))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wInt32" -> Json.jNumber(35544)))) ==
          Wrapper(wInt32 = Some(35544))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wInt32" -> Json.jNumber(0)))) ==
          Wrapper(wInt32 = Some(0))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wInt64" -> Json.jString("125")))) ==
          Wrapper(wInt64 = Some(125))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wUint32" -> Json.jNumber(125)))) ==
          Wrapper(wUint32 = Some(125))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wUint64" -> Json.jString("125")))) ==
          Wrapper(wUint64 = Some(125))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wString" -> Json.jString("bar")))) ==
          Wrapper(wString = Some("bar"))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wString" -> Json.jString("")))) ==
          Wrapper(wString = Some(""))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wBytes" -> Json.jString("AwUE")))) ==
          Wrapper(wBytes = Some(ByteString.copyFrom(Array[Byte](3, 5, 4))))
      )
      assert(
        JsonFormat.fromJson[Wrapper](render(Map("wBytes" -> Json.jString("")))) ==
          Wrapper(wBytes = Some(ByteString.EMPTY))
      )
    }
  }

}
