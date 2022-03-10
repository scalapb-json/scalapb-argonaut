package scalapb_argonaut

import argonaut.Json
import argonaut.Json.JsonArray
import argonaut.JsonNumber
import argonaut.JsonObject
import argonaut.JsonLong
import argonaut.JsonDecimal
import argonaut.JsonBigDecimal
import argonaut.Parse
import com.google.protobuf.struct.Struct
import com.google.protobuf.struct.Value
import scalapb_json.ProtoMacrosCommon._
import scalapb.{GeneratedMessage, GeneratedMessageCompanion}
import scala.quoted.*
import scala.reflect.NameTransformer.MODULE_INSTANCE_NAME
import scala.util.Failure
import scala.util.Success
import scala.util.Try
import scala.util.control.NonFatal

object ProtoMacrosArgonaut {

  implicit val toExprJsonNumber: ToExpr[JsonNumber] =
    new ToExpr[JsonNumber] {
      def apply(j: JsonNumber)(using Quotes) = j match {
        case JsonLong(value) =>
          '{ JsonLong(${ Expr(value) }) }
        case JsonBigDecimal(value) =>
          '{ JsonBigDecimal(${ Expr(value) }) }
        case JsonDecimal(value) =>
          '{ JsonNumber.unsafeDecimal(${ Expr(value) }) }
      }
    }

  implicit val toExprJsonObject: ToExpr[JsonObject] =
    new ToExpr[JsonObject] {
      def apply(j: JsonObject)(using Quotes) = '{
        JsonObject.fromIterable(${ summon[ToExpr[List[(String, Json)]]].apply(j.toList) })
      }
    }

  implicit val toExprJson: ToExpr[Json] =
    new ToExpr[Json] {
      def apply(j: Json)(using Quotes) = j.fold[Expr[Json]](
        jsonNull = '{ Json.jNull },
        jsonBool = value => {
          if (value) '{ Json.jTrue }
          else '{ Json.jFalse }
        },
        jsonNumber = (value: JsonNumber) =>
          '{
            Json.jNumber(${ summon[ToExpr[JsonNumber]].apply(value) })
          },
        jsonString = (value: String) =>
          '{
            Json.jString(${ summon[ToExpr[String]].apply(value) })
          },
        jsonArray = (value: JsonArray) =>
          '{
            Json.jArray(${ summon[ToExpr[List[Json]]].apply(value) })
          },
        jsonObject = (value: JsonObject) =>
          '{
            Json.jObject(${ summon[ToExpr[JsonObject]].apply(value) })
          }
      )
    }

  extension (inline s: StringContext) {
    inline def struct(): com.google.protobuf.struct.Struct =
      ${ structInterpolation('s) }
    inline def value(): com.google.protobuf.struct.Value =
      ${ valueInterpolation('s) }
  }

  extension [A <: GeneratedMessage](inline companion: GeneratedMessageCompanion[A]) {
    inline def fromJsonConstant(inline json: String): A =
      ${
        ProtoMacrosArgonaut.fromJsonConstantImpl[A]('json, 'companion)
      }
  }

  extension [A <: GeneratedMessage](companion: GeneratedMessageCompanion[A]) {
    def fromJson(json: String): A =
      JsonFormat.fromJsonString[A](json)(companion)

    def fromJsonOpt(json: String): Option[A] =
      try {
        Some(fromJson(json))
      } catch {
        case NonFatal(_) =>
          None
      }

    def fromJsonEither(json: String): Either[Throwable, A] =
      try {
        Right(fromJson(json))
      } catch {
        case NonFatal(e) =>
          Left(e)
      }

    def fromJsonTry(json: String): Try[A] =
      try {
        Success(fromJson(json))
      } catch {
        case NonFatal(e) =>
          Failure(e)
      }
  }

  private[this] def structInterpolation(
    s: Expr[StringContext]
  )(using quote: Quotes): Expr[Struct] = {
    import quote.reflect.report
    val Seq(str) = s.valueOrAbort.parts
    val json = Parse
      .parse(str)
      .flatMap(j => j.obj.toRight(s"expect json object but got ${j.name}"))
      .left
      .map(report.errorAndAbort)
      .merge
    Expr(
      StructFormat.structParser(json)
    )
  }

  private[this] def valueInterpolation(s: Expr[StringContext])(using quote: Quotes): Expr[Value] = {
    import quote.reflect.report
    val Seq(str) = s.valueOrAbort.parts
    val json = Parse.parse(str).left.map(report.errorAndAbort).merge
    Expr(
      StructFormat.structValueParser(json)
    )
  }

  private[this] def fromJsonConstantImpl[A <: GeneratedMessage: Type](
    json: Expr[String],
    companion: Expr[GeneratedMessageCompanion[A]]
  )(using quote: Quotes): Expr[A] = {
    import quote.reflect.report
    val str = json.valueOrAbort
    val clazz = Class.forName(Type.show[A] + "$")
    implicit val c: GeneratedMessageCompanion[A] =
      clazz.getField(MODULE_INSTANCE_NAME).get(null).asInstanceOf[GeneratedMessageCompanion[A]]

    val jsonObj = argonaut.Parse.parse(str).left.map(report.errorAndAbort).merge
    // check compile time
    JsonFormat.fromJson[A](jsonObj)
    val expr = summon[ToExpr[Json]].apply(jsonObj)

    '{
      JsonFormat.fromJson[A]($expr)($companion)
    }
  }
}
