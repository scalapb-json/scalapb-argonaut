package scalapb_argonaut

import com.google.protobuf.ByteString
import com.google.protobuf.field_mask.FieldMask
import com.google.protobuf.descriptor.FieldDescriptorProto
import com.google.protobuf.duration.Duration
import com.google.protobuf.struct.NullValue
import argonaut._
import com.google.protobuf.descriptor.FieldDescriptorProto.Type._
import scalapb_json._

import scala.collection.mutable
import scala.reflect.ClassTag
import scalapb._
import scalapb.descriptors._
import scalapb_json.ScalapbJsonCommon.GenericCompanion

import scala.util.control.NonFatal

case class Formatter[T](writer: (Printer, T) => Json, parser: (Parser, Json) => T)

case class FormatRegistry(
  messageFormatters: Map[Class[_], Formatter[_]] = Map.empty,
  enumFormatters: Map[EnumDescriptor, EnumFormatter[EnumValueDescriptor]] = Map.empty,
  registeredCompanions: Seq[GenericCompanion] = Seq.empty
) {
  def registerMessageFormatter[T <: GeneratedMessage](
    writer: (Printer, T) => Json,
    parser: (Parser, Json) => T
  )(implicit ct: ClassTag[T]): FormatRegistry = {
    copy(messageFormatters = messageFormatters + (ct.runtimeClass -> Formatter(writer, parser)))
  }

  def registerEnumFormatter[E <: GeneratedEnum](
    writer: (Printer, EnumValueDescriptor) => Json,
    parser: (Parser, Json) => Option[EnumValueDescriptor]
  )(implicit cmp: GeneratedEnumCompanion[E]): FormatRegistry = {
    copy(enumFormatters = enumFormatters + (cmp.scalaDescriptor -> EnumFormatter(writer, parser)))
  }

  def registerWriter[T <: GeneratedMessage: ClassTag](
    writer: T => Json,
    parser: Json => T
  ): FormatRegistry = {
    registerMessageFormatter((p: Printer, t: T) => writer(t), (p: Parser, v: Json) => parser(v))
  }

  def getMessageWriter[T](klass: Class[_ <: T]): Option[(Printer, T) => Json] = {
    messageFormatters.get(klass).asInstanceOf[Option[Formatter[T]]].map(_.writer)
  }

  def getMessageParser[T](klass: Class[_ <: T]): Option[(Parser, Json) => T] = {
    messageFormatters.get(klass).asInstanceOf[Option[Formatter[T]]].map(_.parser)
  }

  def getEnumWriter(descriptor: EnumDescriptor): Option[(Printer, EnumValueDescriptor) => Json] = {
    enumFormatters.get(descriptor).map(_.writer)
  }

  def getEnumParser(
    descriptor: EnumDescriptor
  ): Option[(Parser, Json) => Option[EnumValueDescriptor]] = {
    enumFormatters.get(descriptor).map(_.parser)
  }
}

class Printer(config: Printer.PrinterConfig) {
  def this() = this(Printer.defaultConfig)
  @deprecated(
    message =
      "Use new Printer() and chain includingDefaultValueFields, preservingProtoFieldNames, etc.",
    since = "0.6.0-M1"
  )
  def this(
    includingDefaultValueFields: Boolean = Printer.defaultConfig.isIncludingDefaultValueFields,
    preservingProtoFieldNames: Boolean = Printer.defaultConfig.isPreservingProtoFieldNames,
    formattingLongAsNumber: Boolean = Printer.defaultConfig.isFormattingLongAsNumber,
    formattingEnumsAsNumber: Boolean = Printer.defaultConfig.isFormattingEnumsAsNumber,
    formatRegistry: FormatRegistry = Printer.defaultConfig.formatRegistry,
    typeRegistry: TypeRegistry = Printer.defaultConfig.typeRegistry
  ) = this(
    Printer.PrinterConfig(
      isIncludingDefaultValueFields = includingDefaultValueFields,
      isPreservingProtoFieldNames = preservingProtoFieldNames,
      isFormattingLongAsNumber = formattingLongAsNumber,
      isFormattingEnumsAsNumber = formattingEnumsAsNumber,
      formatRegistry = formatRegistry,
      typeRegistry = typeRegistry
    )
  )

  def includingDefaultValueFields: Printer =
    new Printer(config.copy(isIncludingDefaultValueFields = true))

  def preservingProtoFieldNames: Printer =
    new Printer(config.copy(isPreservingProtoFieldNames = true))

  def formattingLongAsNumber: Printer =
    new Printer(config.copy(isFormattingLongAsNumber = true))

  def formattingEnumsAsNumber: Printer =
    new Printer(config.copy(isFormattingEnumsAsNumber = true))

  def withFormatRegistry(formatRegistry: FormatRegistry): Printer =
    new Printer(config.copy(formatRegistry = formatRegistry))

  def withTypeRegistry(typeRegistry: TypeRegistry): Printer =
    new Printer(config.copy(typeRegistry = typeRegistry))

  def typeRegistry: TypeRegistry = config.typeRegistry

  def print[A](m: GeneratedMessage): String = {
    toJson(m).toString()
  }

  private[this] type JField = (String, Json)
  private[this] type FieldBuilder = mutable.Builder[JField, List[JField]]

  private[this] def JField(key: String, value: Json) = (key, value)

  private def serializeMessageField(
    fd: FieldDescriptor,
    name: String,
    value: Any,
    b: FieldBuilder
  ): Unit = {
    value match {
      case null =>
      // We are never printing empty optional messages to prevent infinite recursion.
      case Nil =>
        if (config.isIncludingDefaultValueFields) {
          b += ((name, if (fd.isMapField) Json.obj() else Json.jEmptyArray))
        }
      case xs: Iterable[GeneratedMessage] @unchecked =>
        if (fd.isMapField) {
          val mapEntryDescriptor = fd.scalaType.asInstanceOf[ScalaType.Message].descriptor
          val keyDescriptor = mapEntryDescriptor.findFieldByNumber(1).get
          val valueDescriptor = mapEntryDescriptor.findFieldByNumber(2).get
          b += JField(
            name,
            Json.obj(xs.map { x =>
              val key = x.getField(keyDescriptor) match {
                case PBoolean(v) => v.toString
                case PDouble(v) => v.toString
                case PFloat(v) => v.toString
                case PInt(v) => v.toString
                case PLong(v) => v.toString
                case PString(v) => v
                case v => throw new JsonFormatException(s"Unexpected value for key: $v")
              }
              val value = if (valueDescriptor.protoType.isTypeMessage) {
                toJson(x.getFieldByNumber(valueDescriptor.number).asInstanceOf[GeneratedMessage])
              } else {
                serializeSingleValue(
                  valueDescriptor,
                  x.getField(valueDescriptor),
                  config.isFormattingLongAsNumber
                )
              }
              key -> value
            }.toList: _*)
          )
        } else {
          b += JField(name, Json.jArray(xs.iterator.map(toJson).toList))
        }
      case msg: GeneratedMessage =>
        b += JField(name, toJson(msg))
      case v =>
        throw new JsonFormatException(v.toString)
    }
  }

  private def serializeNonMessageField(
    fd: FieldDescriptor,
    name: String,
    value: PValue,
    b: FieldBuilder
  ) = {
    value match {
      case PEmpty =>
        if (config.isIncludingDefaultValueFields && fd.containingOneof.isEmpty) {
          b += JField(name, defaultJson(fd))
        }
      case PRepeated(xs) =>
        if (xs.nonEmpty || config.isIncludingDefaultValueFields) {
          b += JField(
            name,
            Json.array(
              xs.map(serializeSingleValue(fd, _, config.isFormattingLongAsNumber)): _*
            )
          )
        }
      case v =>
        if (config.isIncludingDefaultValueFields ||
          !fd.isOptional ||
          !fd.file.isProto3 ||
          (v != scalapb_json.ScalapbJsonCommon.defaultValue(fd)) ||
          fd.containingOneof.isDefined) {
          b += JField(name, serializeSingleValue(fd, v, config.isFormattingLongAsNumber))
        }
    }
  }

  def toJson[A <: GeneratedMessage](m: A): Json = {
    config.formatRegistry.getMessageWriter[A](m.getClass) match {
      case Some(f) => f(this, m)
      case None =>
        val b = List.newBuilder[JField]
        val descriptor = m.companion.scalaDescriptor
        b.sizeHint(descriptor.fields.size)
        descriptor.fields.foreach { f =>
          val name =
            if (config.isPreservingProtoFieldNames) f.name
            else scalapb_json.ScalapbJsonCommon.jsonName(f)
          if (f.protoType.isTypeMessage) {
            serializeMessageField(f, name, m.getFieldByNumber(f.number), b)
          } else {
            serializeNonMessageField(f, name, m.getField(f), b)
          }
        }
        Json.obj(b.result(): _*)
    }
  }

  private def defaultJson(fd: FieldDescriptor): Json =
    serializeSingleValue(
      fd,
      scalapb_json.ScalapbJsonCommon.defaultValue(fd),
      config.isFormattingLongAsNumber
    )

  private def unsignedLong(n: Long) =
    if (n < 0) BigDecimal(BigInt(n & 0X7FFFFFFFFFFFFFFFL).setBit(63)) else BigDecimal(n)

  private def formatLong(
    n: Long,
    protoType: FieldDescriptorProto.Type,
    formattingLongAsNumber: Boolean
  ): Json = {
    val v =
      if (protoType.isTypeUint64 || protoType.isTypeFixed64) unsignedLong(n) else BigDecimal(n)
    if (formattingLongAsNumber) Json.jNumber(v) else Json.jString(v.toString())
  }

  def serializeSingleValue(
    fd: FieldDescriptor,
    value: PValue,
    formattingLongAsNumber: Boolean
  ): Json = value match {
    case PEnum(e) =>
      config.formatRegistry.getEnumWriter(e.containingEnum) match {
        case Some(writer) => writer(this, e)
        case None =>
          if (config.isFormattingEnumsAsNumber) Json.jNumber(e.number) else Json.jString(e.name)
      }
    case PInt(v) if fd.protoType.isTypeUint32 => Json.jNumber(ScalapbJsonCommon.unsignedInt(v))
    case PInt(v) if fd.protoType.isTypeFixed32 => Json.jNumber(ScalapbJsonCommon.unsignedInt(v))
    case PInt(v) => Json.jNumber(v)
    case PLong(v) => formatLong(v, fd.protoType, formattingLongAsNumber)
    case PDouble(v) =>
      if (v.isPosInfinity) {
        JsStringPosInfinity
      } else if (v.isNegInfinity) {
        JsStringNegInfinity
      } else if (java.lang.Double.isNaN(v)) {
        JsStringNaN
      } else {
        Json.jNumber(v)
      }
    case PFloat(v) =>
      if (v.isPosInfinity) {
        JsStringPosInfinity
      } else if (v.isNegInfinity) {
        JsStringNegInfinity
      } else if (java.lang.Double.isNaN(v)) {
        JsStringNaN
      } else {
        Json.jNumber(BigDecimal(v.toString))
      }
    case PBoolean(v) => Json.jBool(v)
    case PString(v) => Json.jString(v)
    case PByteString(v) => Json.jString(java.util.Base64.getEncoder.encodeToString(v.toByteArray))
    case _: PMessage | PRepeated(_) | PEmpty => throw new RuntimeException("Should not happen")
  }

  private[this] val JsStringPosInfinity = Json.jString("Infinity")
  private[this] val JsStringNegInfinity = Json.jString("-Infinity")
  private[this] val JsStringNaN = Json.jString("NaN")
}

object Parser {
  private final case class ParserConfig(
    isIgnoringUnknownFields: Boolean,
    formatRegistry: FormatRegistry,
    typeRegistry: TypeRegistry
  )

  private def defaultConfig: ParserConfig = Parser.ParserConfig(
    isIgnoringUnknownFields = false,
    JsonFormat.DefaultRegistry,
    TypeRegistry.empty
  )

  private val memorizedFieldNameMap = new MemorizedFieldNameMap()
}

class Parser(config: Parser.ParserConfig) {
  def this() = this(Parser.defaultConfig)

  @deprecated(
    message = "Use new Parser() and chain with usingTypeRegistry or formatRegistry",
    since = "0.6.0-M1"
  )
  def this(
    preservingProtoFieldNames: Boolean = false,
    formatRegistry: FormatRegistry = JsonFormat.DefaultRegistry,
    typeRegistry: TypeRegistry = TypeRegistry.empty
  ) =
    this(
      Parser.ParserConfig(
        isIgnoringUnknownFields = false,
        formatRegistry,
        typeRegistry
      )
    )

  def ignoringUnknownFields: Parser =
    new Parser(config.copy(isIgnoringUnknownFields = true))

  def withFormatRegistry(formatRegistry: FormatRegistry) =
    new Parser(config.copy(formatRegistry = formatRegistry))

  def withTypeRegistry(typeRegistry: TypeRegistry) =
    new Parser(config.copy(typeRegistry = typeRegistry))

  def typeRegistry: TypeRegistry = config.typeRegistry

  def fromJsonString[A <: GeneratedMessage](
    str: String
  )(implicit cmp: GeneratedMessageCompanion[A]): A = {
    JsonParser.parse(str) match {
      case Left(e) =>
        throw JsonFormatException(s"could not parse json $e", null)
      case Right(x) =>
        fromJson(x)
    }
  }

  def fromJson[A <: GeneratedMessage](
    value: Json
  )(implicit cmp: GeneratedMessageCompanion[A]): A = {
    fromJson(value, skipTypeUrl = false)
  }

  private[scalapb_argonaut] def fromJson[A <: GeneratedMessage](
    value: Json,
    skipTypeUrl: Boolean
  )(implicit cmp: GeneratedMessageCompanion[A]): A = {
    cmp.messageReads.read(fromJsonToPMessage(cmp, value, skipTypeUrl))
  }

  private def serializedName(fd: FieldDescriptor): String = {
    if (config.isIgnoringUnknownFields) fd.asProto.getName
    else scalapb_json.ScalapbJsonCommon.jsonName(fd)
  }

  private def fromJsonToPMessage(
    cmp: GeneratedMessageCompanion[_],
    value: Json,
    skipTypeUrl: Boolean
  ): PMessage = {
    def parseValue(fd: FieldDescriptor, value: Json): PValue = {
      if (fd.isMapField) {
        value.obj match {
          case Some(vals) =>
            val mapEntryDesc = fd.scalaType.asInstanceOf[ScalaType.Message].descriptor
            val keyDescriptor = mapEntryDesc.findFieldByNumber(1).get
            val valueDescriptor = mapEntryDesc.findFieldByNumber(2).get
            PRepeated(vals.toList.iterator.map {
              case (key, jValue) =>
                val keyObj = keyDescriptor.scalaType match {
                  case ScalaType.Boolean => PBoolean(java.lang.Boolean.valueOf(key))
                  case ScalaType.Double => PDouble(java.lang.Double.valueOf(key))
                  case ScalaType.Float => PFloat(java.lang.Float.valueOf(key))
                  case ScalaType.Int => PInt(java.lang.Integer.valueOf(key))
                  case ScalaType.Long => PLong(java.lang.Long.valueOf(key))
                  case ScalaType.String => PString(key)
                  case _ => throw new RuntimeException(s"Unsupported type for key for ${fd.name}")
                }
                PMessage(
                  Map(
                    keyDescriptor -> keyObj,
                    valueDescriptor -> parseSingleValue(
                      cmp.messageCompanionForFieldNumber(fd.number),
                      valueDescriptor,
                      jValue
                    )
                  )
                )
            }.toVector)
          case _ =>
            throw new JsonFormatException(
              s"Expected an object for map field ${serializedName(fd)} of ${fd.containingMessage.name}"
            )
        }
      } else if (fd.isRepeated) {
        value.array match {
          case Some(vals) =>
            PRepeated(vals.map(parseSingleValue(cmp, fd, _)).toVector)
          case _ =>
            throw new JsonFormatException(
              s"Expected an array for repeated field ${serializedName(fd)} of ${fd.containingMessage.name}"
            )
        }
      } else parseSingleValue(cmp, fd, value)
    }

    config.formatRegistry.getMessageParser(cmp.defaultInstance.getClass) match {
      case Some(p) => p(this, value).asInstanceOf[GeneratedMessage].toPMessage
      case None =>
        value.obj match {
          case Some(fields) =>
            val fieldMap = Parser.memorizedFieldNameMap.get(cmp.scalaDescriptor)
            val valueMapBuilder = Map.newBuilder[FieldDescriptor, PValue]
            fields.toList.foreach {
              case (name, jValue) =>
                if (fieldMap.contains(name)) {
                  if (!jValue.isNull) {
                    val fd = fieldMap(name)
                    valueMapBuilder += (fd -> parseValue(fd, jValue))
                  }
                } else if (!config.isIgnoringUnknownFields && !(skipTypeUrl && name == "@type")) {
                  throw new JsonFormatException(
                    s"Cannot find field: ${name} in message ${cmp.scalaDescriptor.fullName}"
                  )
                }
            }

            PMessage(valueMapBuilder.result())
          case _ =>
            throw new JsonFormatException(s"Expected an object, found ${value}")
        }
    }
  }

  def defaultEnumParser(
    enumDescriptor: EnumDescriptor,
    value: Json
  ): Option[EnumValueDescriptor] = {
    def enumValueFromInt(v: Int): Option[EnumValueDescriptor] =
      if (enumDescriptor.file.isProto3)
        Some(enumDescriptor.findValueByNumberCreatingIfUnknown(v))
      else
        enumDescriptor.findValueByNumber(v)

    def fail() =
      throw new JsonFormatException(
        s"Invalid enum value: $value for enum type: ${enumDescriptor.fullName}"
      )

    def defaultValue =
      if (config.isIgnoringUnknownFields && enumDescriptor.file.isProto3)
        enumDescriptor.findValueByNumber(0)
      else None

    val res = value.number match {
      case Some(v) =>
        try {
          enumValueFromInt(v.toBigDecimal.bigDecimal.intValueExact)
        } catch {
          case _: ArithmeticException => defaultValue
        }

      case _ =>
        value.string match {
          case Some(s) =>
            enumDescriptor.values
              .find(_.name == s)
              .orElse {
                try {
                  enumValueFromInt(new java.math.BigDecimal(s).intValueExact())
                } catch {
                  case _: ArithmeticException => None
                  case _: NumberFormatException => None
                }
              }
              .orElse(defaultValue)
          case _ =>
            fail()
        }
    }

    if (res.isEmpty && !config.isIgnoringUnknownFields) {
      fail()
    }

    res
  }

  protected def parseSingleValue(
    containerCompanion: GeneratedMessageCompanion[_],
    fd: FieldDescriptor,
    value: Json
  ): PValue = fd.scalaType match {
    case ScalaType.Enum(ed) => {
      {
        config.formatRegistry.getEnumParser(ed) match {
          case Some(parser) =>
            parser(this, value)
          case None =>
            defaultEnumParser(ed, value)
        }
      } match {
        case Some(x) => PEnum(x)
        case None => PEmpty
      }
    }
    case ScalaType.Message(_) =>
      fromJsonToPMessage(
        containerCompanion.messageCompanionForFieldNumber(fd.number),
        value,
        skipTypeUrl = false
      )
    case st =>
      JsonFormat.parsePrimitive(
        fd.protoType,
        value,
        throw new JsonFormatException(
          s"Unexpected value ($value) for field ${serializedName(fd)} of ${fd.containingMessage.name}"
        )
      )
  }
}

object Printer {
  private final case class PrinterConfig(
    isIncludingDefaultValueFields: Boolean,
    isPreservingProtoFieldNames: Boolean,
    isFormattingLongAsNumber: Boolean,
    isFormattingEnumsAsNumber: Boolean,
    formatRegistry: FormatRegistry,
    typeRegistry: TypeRegistry
  )

  private val defaultConfig = PrinterConfig(
    isIncludingDefaultValueFields = false,
    isPreservingProtoFieldNames = false,
    isFormattingLongAsNumber = false,
    isFormattingEnumsAsNumber = false,
    formatRegistry = JsonFormat.DefaultRegistry,
    typeRegistry = TypeRegistry.empty
  )
}

object JsonFormat {
  import com.google.protobuf.wrappers
  import scalapb_json.ScalapbJsonCommon._

  val DefaultRegistry = ScalapbArgonautPlatform
    .registerPlatformWriters(FormatRegistry())
    .registerWriter(
      (d: Duration) => Json.jString(Durations.writeDuration(d)), {
        _.string match {
          case Some(str) =>
            Durations.parseDuration(str)
          case _ =>
            throw new JsonFormatException("Expected a string.")
        }
      }
    )
    .registerWriter(
      (f: FieldMask) => Json.jString(scalapb.FieldMaskUtil.toJsonString(f)), {
        _.string match {
          case Some(str) =>
            scalapb.FieldMaskUtil.fromJsonString(str)
          case _ =>
            throw new JsonFormatException("Expected a string.")
        }
      }
    )
    .registerMessageFormatter[wrappers.DoubleValue](
      primitiveWrapperWriter,
      primitiveWrapperParser[wrappers.DoubleValue]
    )
    .registerMessageFormatter[wrappers.FloatValue](
      primitiveWrapperWriter,
      primitiveWrapperParser[wrappers.FloatValue]
    )
    .registerMessageFormatter[wrappers.Int32Value](
      primitiveWrapperWriter,
      primitiveWrapperParser[wrappers.Int32Value]
    )
    .registerMessageFormatter[wrappers.Int64Value](
      primitiveWrapperWriter,
      primitiveWrapperParser[wrappers.Int64Value]
    )
    .registerMessageFormatter[wrappers.UInt32Value](
      primitiveWrapperWriter,
      primitiveWrapperParser[wrappers.UInt32Value]
    )
    .registerMessageFormatter[wrappers.UInt64Value](
      primitiveWrapperWriter,
      primitiveWrapperParser[wrappers.UInt64Value]
    )
    .registerMessageFormatter[wrappers.BoolValue](
      primitiveWrapperWriter,
      primitiveWrapperParser[wrappers.BoolValue]
    )
    .registerMessageFormatter[wrappers.BytesValue](
      primitiveWrapperWriter,
      primitiveWrapperParser[wrappers.BytesValue]
    )
    .registerMessageFormatter[wrappers.StringValue](
      primitiveWrapperWriter,
      primitiveWrapperParser[wrappers.StringValue]
    )
    .registerEnumFormatter[NullValue](
      (_, _) => Json.jNull,
      (parser, value) => {
        if (value.isNull) {
          Some(NullValue.NULL_VALUE.scalaValueDescriptor)
        } else {
          parser.defaultEnumParser(NullValue.scalaDescriptor, value)
        }
      }
    )
    .registerWriter[com.google.protobuf.struct.Value](
      StructFormat.structValueWriter,
      StructFormat.structValueParser
    )
    .registerWriter[com.google.protobuf.struct.Struct](
      StructFormat.structWriter,
      StructFormat.structParser
    )
    .registerWriter[com.google.protobuf.struct.ListValue](
      x => Json.array(StructFormat.listValueWriter(x): _*),
      StructFormat.listValueParser
    )
    .registerMessageFormatter[com.google.protobuf.any.Any](AnyFormat.anyWriter, AnyFormat.anyParser)

  def primitiveWrapperWriter[T <: GeneratedMessage](
    implicit cmp: GeneratedMessageCompanion[T]
  ): ((Printer, T) => Json) = {
    val fieldDesc = cmp.scalaDescriptor.findFieldByNumber(1).get
    (printer, t) =>
      printer.serializeSingleValue(fieldDesc, t.getField(fieldDesc), formattingLongAsNumber = false)
  }

  def primitiveWrapperParser[T <: GeneratedMessage](
    implicit cmp: GeneratedMessageCompanion[T]
  ): ((Parser, Json) => T) = {
    val fieldDesc = cmp.scalaDescriptor.findFieldByNumber(1).get
    (parser, jv) =>
      cmp.messageReads.read(
        PMessage(
          Map(
            fieldDesc -> JsonFormat.parsePrimitive(
              fieldDesc.protoType,
              jv,
              throw new JsonFormatException(s"Unexpected value for ${cmp.scalaDescriptor.name}")
            )
          )
        )
      )
  }

  val printer = new Printer()
  val parser = new Parser()

  def toJsonString[A <: GeneratedMessage](m: A): String = printer.print(m)

  def toJson[A <: GeneratedMessage](m: A): Json = printer.toJson(m)

  def fromJson[A <: GeneratedMessage: GeneratedMessageCompanion](value: Json): A = {
    parser.fromJson(value)
  }

  def fromJsonString[A <: GeneratedMessage: GeneratedMessageCompanion](
    str: String
  ): A = {
    parser.fromJsonString(str)
  }

  implicit def protoToDecodeJson[T <: GeneratedMessage: GeneratedMessageCompanion]: DecodeJson[T] =
    DecodeJson { value =>
      try {
        DecodeResult.ok(parser.fromJson(value.focus))
      } catch {
        case NonFatal(e) =>
          DecodeResult.fail(e.toString, value.history)
      }
    }

  implicit def protoToEncodeJson[T <: GeneratedMessage]: EncodeJson[T] =
    EncodeJson(printer.toJson(_))

  @deprecated("Use parsePrimitive(protoType, value, onError) instead.", "0.6.0-M1")
  def parsePrimitive(
    scalaType: ScalaType,
    protoType: FieldDescriptorProto.Type,
    value: Json,
    onError: => PValue
  ): PValue =
    parsePrimitive(protoType, value, onError)

  def parsePrimitive(
    protoType: FieldDescriptorProto.Type,
    value: Json,
    onError: => PValue
  ): PValue = {
    protoType match {
      case TYPE_UINT32 | TYPE_FIXED32 =>
        value.fold(
          jsonNull = onError,
          jsonBool = _ => onError,
          jsonNumber = x => parseUint32(x.toBigDecimal.toString()),
          jsonString = x => parseUint32(x),
          jsonArray = _ => onError,
          jsonObject = _ => onError
        )
      case TYPE_SINT32 | TYPE_INT32 | TYPE_SFIXED32 =>
        value.fold(
          jsonNull = onError,
          jsonBool = _ => onError,
          jsonNumber = x => parseInt32(x.toBigDecimal.toString()),
          jsonString = x => parseInt32(x),
          jsonArray = x => onError,
          jsonObject = x => onError
        )
      case TYPE_UINT64 | TYPE_FIXED64 =>
        value.fold(
          jsonNull = onError,
          jsonBool = _ => onError,
          jsonNumber = x => parseUint64(x.toBigDecimal.toString),
          jsonString = parseUint64,
          jsonArray = _ => onError,
          jsonObject = _ => onError
        )
      case TYPE_SINT64 | TYPE_INT64 | TYPE_SFIXED64 =>
        value.fold(
          jsonNull = onError,
          jsonBool = _ => onError,
          jsonNumber = x => parseInt64(x.toBigDecimal.toString),
          jsonString = parseInt64,
          jsonArray = _ => onError,
          jsonObject = _ => onError
        )
      case TYPE_DOUBLE =>
        value.fold(
          jsonNull = onError,
          jsonBool = _ => onError,
          jsonNumber = x => parseDouble(x.toBigDecimal.toString),
          jsonString = parseDouble,
          jsonArray = _ => onError,
          jsonObject = _ => onError
        )
      case TYPE_FLOAT =>
        value.fold(
          jsonNull = onError,
          jsonBool = _ => onError,
          jsonNumber = x => parseFloat(x.toBigDecimal.toString),
          jsonString = parseFloat,
          jsonArray = _ => onError,
          jsonObject = _ => onError
        )
      case TYPE_BOOL =>
        value.bool match {
          case Some(i) =>
            PBoolean(i)
          case None =>
            value.string match {
              case Some("true") =>
                PBoolean(true)
              case Some("false") =>
                PBoolean(false)
              case _ =>
                onError
            }
        }
      case TYPE_STRING =>
        value.string match {
          case Some(i) => PString(i)
          case None => onError
        }
      case TYPE_BYTES =>
        value.string match {
          case Some(s) =>
            PByteString(ByteString.copyFrom(java.util.Base64.getDecoder.decode(s)))
          case None =>
            onError
        }
      case TYPE_ENUM | TYPE_GROUP | TYPE_MESSAGE | Unrecognized(_) =>
        onError
    }
  }
}
