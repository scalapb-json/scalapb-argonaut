package scalapb_argonaut

import scala.compiletime.testing.{typeCheckErrors, ErrorKind}
import org.scalatest.funspec.AnyFunSpec
import scalapb_argonaut.ProtoMacrosArgonaut._

class ProtoMacrosArgonautTest2 extends AnyFunSpec {

  describe("ProtoMacrosArgonaut scala 3 test") {
    inline def checkTypeError(
      src: String,
      expectMessage: String
    ) = {
      typeCheckErrors(src) match {
        case List(e) =>
          assert(e.kind == ErrorKind.Typer)
          assert(e.message == expectMessage)
        case other =>
          fail("unexpected " + other)
      }
    }

    it("struct") {
      checkTypeError(""" struct"null" """, "expect json object but got Null")
      checkTypeError(""" struct"[3]" """, "expect json object but got Array")
      checkTypeError(""" struct"true" """, "expect json object but got Boolean")
      checkTypeError(""" struct"12345" """, "expect json object but got Number")

      checkTypeError(""" struct" ] " """, "Unexpected content found: ] ")
    }

    it("value") {
      checkTypeError(""" value" ] " """, "Unexpected content found: ] ")
      checkTypeError(""" value" } " """, "Unexpected content found: } ")
    }
  }
}
