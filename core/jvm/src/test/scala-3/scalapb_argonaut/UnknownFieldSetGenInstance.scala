package scalapb_argonaut

import com.google.protobuf.ByteString
import scalapb.UnknownFieldSet
import scalaprops.Gen
import scalaprops.ScalapropsShapeless.given
import RepeatableTestGen.Base._

object UnknownFieldSetGenInstance {
  private[this] implicit val byteStringGen: Gen[ByteString] =
    Gen.alphaNumString.map(ByteString.copyFromUtf8)

  val value: Gen[UnknownFieldSet] = Gen[UnknownFieldSet]
}
