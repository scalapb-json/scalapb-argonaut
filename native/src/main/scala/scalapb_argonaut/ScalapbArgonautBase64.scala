package scalapb_argonaut

import scala.annotation.tailrec
import java.nio.charset.StandardCharsets

// https://github.com/scala-js/scala-js/blob/6169a651ad7/javalib/src/main/scala/java/util/Base64.scala
private[scalapb_argonaut] object ScalapbArgonautBase64 {

  private val chars = ('A' to 'Z') ++ ('a' to 'z') ++ ('0' to '9')

  private val basicEncodeTable: Array[Byte] =
    (chars ++ Seq('+', '/')).map(_.toByte).toArray

  private def decodeTable(encode: Array[Byte]): Array[Int] = {
    val decode = Array.fill[Int](256)(-1)
    for ((b, i) <- encode.zipWithIndex)
      decode(b) = i
    decode('=') = -2
    decode
  }

  private val basicDecodeTable = decodeTable(basicEncodeTable)

  private final val mimeLineLength = 76

  private val basicEncoder =
    new Encoder(basicEncodeTable)

  private val basicDecoder =
    new Decoder(basicDecodeTable, ignoreInvalid = false)

  // --------------------------------------------------------------------------

  def getEncoder(): Encoder = basicEncoder

  def getDecoder(): Decoder = basicDecoder

  // --------------------------------------------------------------------------

  class Decoder private[ScalapbArgonautBase64] (table: Array[Int], ignoreInvalid: Boolean) {

    def decode(src: Array[Byte]): Array[Byte] = {
      val dst = new Array[Byte](dstRequiredLength(src))
      doDecode(new Wrapper(src), new Wrapper(dst))
      dst
    }

    def decode(src: String): Array[Byte] =
      decode(src.getBytes(StandardCharsets.ISO_8859_1))

    def decode(src: Array[Byte], dst: Array[Byte]): Int = {
      if (dst.length < dstMaxLength(src.length) && // dst is possibly too small
        dst.length < dstRequiredLength(src)) { // dst is actually too small
        throw new IllegalArgumentException(
          "Output byte array is too small for decoding all input bytes")
      }

      doDecode(new Wrapper(src), new Wrapper(dst))
    }

    // ------------------------------------------------------------------------
    // PRIVATE
    // ------------------------------------------------------------------------

    private def doDecode(src: Wrapper, dst: Wrapper): Int = {
      val srcBuffer = new Wrapper(new Array[Byte](4))

      @inline
      def inputData(): Unit = {
        srcBuffer.position = 0
        var shift = 18
        var i = 0
        while (srcBuffer.hasRemaining) {
          i |= ((srcBuffer.get() & 0xff) << shift)
          shift -= 6
        }

        if (shift == 12) {
          throw new IllegalArgumentException("Last unit does not have enough valid bits")
        }

        if (shift <= 6)
          dst.put((i >> 16).toByte)
        if (shift <= 0)
          dst.put((i >> 8).toByte)
        if (shift <= -6)
          dst.put(i.toByte)
        srcBuffer.clear()
      }

      @tailrec
      def iterate(): Unit = {
        if (src.hasRemaining) {
          if (srcBuffer.hasRemaining) {
            val int = src.get() & 0xff
            table(int) match {
              case -2 =>
              case -1 =>
                if (!ignoreInvalid) {
                  throw new IllegalArgumentException("Illegal base64 character " + int.toHexString)
                }
                iterate()

              case i =>
                srcBuffer.put(i.toByte)
                iterate()
            }
          } else {
            inputData()
            iterate()
          }
        }
      }

      iterate()

      // end or padding
      srcBuffer.flip()
      inputData()
      while (src.hasRemaining) {
        val int = src.get() & 0xff
        val value = table(int)
        if (value != -2 && (!ignoreInvalid || value > 0)) {
          throw new IllegalArgumentException(s"Input byte array has incorrect ending byte at $int")
        }
      }

      dst.position
    }

    private def dstRequiredLength(src: Array[Byte]): Int = {
      var validBytes = 0

      if (ignoreInvalid) {
        for (i <- src.indices) {
          if (table(src(i) & 0xff) >= 0)
            validBytes += 1
        }
      } else {
        /* We check the end for padding and compute the length from there.
         * This is ok, if the rest contains garbage we'll have written
         * something before throwing but the spec says "If the input byte array
         * is not in valid Base64 encoding scheme then some bytes may have been
         * written to the output byte array before IllegalArgumentException is
         * thrown."
         */
        validBytes = src.length
        if (src.length >= 1 && src.last == '=') {
          validBytes -= 1
          if (src.length >= 2 && src(src.length - 2) == '=')
            validBytes -= 1
        }

        if (src.length >= 1 && validBytes == 0) {
          throw new IllegalArgumentException("Input byte array has wrong 4-byte ending unit")
        }
      }

      dstMaxLength(validBytes)
    }

    /** Computes the destination length solely based on the source length,
     *  without knowing about padding.
     */
    private def dstMaxLength(srcLength: Int): Int =
      (srcLength + 3) / 4 * 3 - (if (srcLength % 4 == 0) 0
                                 else 4 - (srcLength % 4))

  }

  // --------------------------------------------------------------------------

  class Encoder private[ScalapbArgonautBase64] (
    table: Array[Byte],
    lineLength: Int = 0,
    lineSeparator: Array[Byte] = Array.empty,
    withPadding: Boolean = true) {

    def encode(src: Array[Byte]): Array[Byte] = {
      val dst = new Array[Byte](dstLength(src.length))
      doEncode(src, dst, dst.length)
      dst
    }

    def encode(src: Array[Byte], dst: Array[Byte]): Int = {
      val dstLen = dstLength(src.length)
      if (dst.length < dstLen) {
        throw new IllegalArgumentException(
          "Output byte array is too small for encoding all input bytes")
      }
      doEncode(src, dst, dstLen)
    }

    def encodeToString(src: Array[Byte]): String =
      new String(encode(src), StandardCharsets.ISO_8859_1)

    // ------------------------------------------------------------------------
    // PRIVATE
    // ------------------------------------------------------------------------

    private def doEncode(src: Array[Byte], dst: Array[Byte], dstLength: Int): Int = {
      doEncode(new Wrapper(src), new Wrapper(dst, 0, dstLength))
    }

    // dst position must always be 0 here
    private def doEncode(src: Wrapper, dst: Wrapper): Int = {
      val length = src.remaining
      var currentLine = 0

      @inline
      def encode(a: Byte, b: Byte, c: Byte): Unit = {
        val bits = (a & 0xff) << 16 | (b & 0xff) << 8 | (c & 0xff)
        dst.put(table((bits >>> 18) & 0x3f))
        dst.put(table((bits >>> 12) & 0x3f))
        if (dst.hasRemaining)
          dst.put(table((bits >>> 6) & 0x3f))
        if (dst.hasRemaining)
          dst.put(table(bits & 0x3f))

        currentLine += 4
        if (lineSeparator.length > 0 && lineLength > 0 &&
          currentLine == lineLength && dst.hasRemaining) {
          lineSeparator.foreach(dst.put(_))
          currentLine = 0
        }
      }

      while (src.remaining >= 3) encode(src.get(), src.get(), src.get())

      (length % 3) match {
        case 0 =>
        case 1 =>
          encode(src.get(), 0, 0)
          if (withPadding) {
            dst.position = dst.position - 2
            dst.put('='.toByte)
            dst.put('='.toByte)
          }
        case 2 =>
          encode(src.get(), src.get(), 0)
          if (withPadding) {
            dst.position = dst.position - 1
            dst.put('='.toByte)
          }
      }

      dst.position
    }

    private def dstLength(srcLength: Int): Int = {
      val withPad = ((srcLength + 2) / 3) * 4
      val toRemove = if (withPadding) 0 else (3 - (srcLength % 3)) % 3
      val withoutEndLines = withPad - toRemove
      val endLines =
        if (lineLength <= 0) 0
        else ((withoutEndLines - 1) / lineLength) * lineSeparator.length
      withoutEndLines + endLines
    }
  }

  // --------------------------------------------------------------------------

  /** An Array augmented with a position and a limit.
   *
   *  This is modeled after `java.nio.ByteBuffer`, but is more lightweight.
   */
  private class Wrapper(array: Array[Byte], var position: Int, var limit: Int) {

    def this(array: Array[Byte]) = this(array, 0, array.length)

    def hasRemaining: Boolean = position < limit

    def remaining: Int = limit - position

    def put(b: Byte): Unit = {
      array(position) = b
      position += 1
    }

    def get(): Byte = {
      position += 1
      array(position - 1)
    }

    def clear(): Unit = {
      position = 0
      limit = array.length
    }

    def flip(): Unit = {
      limit = position
      position = 0
    }
  }
}
