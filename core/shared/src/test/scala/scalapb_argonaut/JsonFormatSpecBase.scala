package scalapb_argonaut

import scalapb_json.JsonFormatException
import jsontest.test._
import utest.TestSuite
import scala.reflect.ClassTag
import JsonFormatSpecBase.assertThrows

object JsonFormatSpecBase {
  def assertThrows[A](f: => Any)(implicit A: ClassTag[A]) = {
    try {
      f
      throw new RuntimeException(s"expect ${A}")
    } catch {
      case _: A =>
      // success
    }
  }
}

trait JsonFormatSpecBase extends JavaAssertions { self: TestSuite =>

  def assertAcceptsQuotes(field: String, value: String): Unit = {
    JsonFormat.fromJsonString[TestAllTypes](s"""{"$field": "$value"}""")
  }
  def assertAcceptsNoQuotes(field: String, value: String): Unit = {
    JsonFormat.fromJsonString[TestAllTypes](s"""{"$field": $value}""")
  }
  def assertAccepts(field: String, value: String): Unit = {
    assertAcceptsQuotes(field, value)
    assertAcceptsNoQuotes(field, value)
  }
  def assertRejectsNoQuotes(field: String, value: String): Unit = {
    assertThrows[JsonFormatException] {
      JsonFormat.fromJsonString[TestAllTypes](s"""{"$field": $value}""")
    }
  }
  def assertRejectsQuotes(field: String, value: String): Unit = {
    assertThrows[JsonFormatException] {
      JsonFormat.fromJsonString[TestAllTypes](s"""{"$field": "$value"}""")
    }
  }
  def assertRejects(field: String, value: String): Unit = {
    assertRejectsNoQuotes(field, value)
    assertRejectsQuotes(field, value)
  }

}
