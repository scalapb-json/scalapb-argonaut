package scalapb_argonaut

import scalapb_json.ScalapbJsonCommon.GenericCompanion
import scalapb.GeneratedMessageCompanion
import scalapb_json._

trait JavaAssertions extends JavaAssertionsPlatform { self: utest.TestSuite =>

  def registeredCompanions: Seq[GeneratedMessageCompanion[?]] = Seq.empty

  val ScalaTypeRegistry = registeredCompanions.foldLeft(TypeRegistry.empty)((r, c) =>
    r.addMessageByCompanion(c.asInstanceOf[GenericCompanion])
  )
  val ScalaJsonParser = new Parser(typeRegistry = ScalaTypeRegistry)
  val ScalaJsonPrinter = new Printer(typeRegistry = ScalaTypeRegistry)
}
