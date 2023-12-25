package scalapb_argonaut

import com.google.protobuf.any.{Any => PBAny}
import utest._
import jsontest.anytests.AnyTest
import scalapb.GeneratedMessageCompanion

object AnyFormatSpecJVM extends TestSuite with JavaAssertions {
  override def registeredCompanions: Seq[GeneratedMessageCompanion[?]] = Seq(AnyTest)

  override val tests = Tests {
    "Any should be serialized the same as in Java (and parsed back to original)" - {
      val RawExample = AnyTest("test")
      val AnyExample = PBAny.pack(RawExample)
      assertJsonIsSameAsJava(AnyExample)
    }
  }
}
