/*
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package wvlet.airframe.json

import wvlet.log.LogSupport

/**
  *
  */
object JSON extends LogSupport {

  /**
    * Parse JSON object or array
    */
  def parse(s: String): JSONValue = {
    parse(JSONSource.fromString(s))
  }

  /**
    * Parse JSON object or array
    */
  def parse(s: Array[Byte]): JSONValue = {
    parse(JSONSource.fromBytes(s))
  }

  /**
    * Parse JSON object or array
    */
  def parse(s: JSONSource): JSONValue = {
    val b = new JSONValueBuilder().singleContext(s, 0)
    JSONScanner.scan(s, b)
    val j = b.result
    j
  }

  /**
    * Parse any json values including null
    */
  def parseAny(s: String): JSONValue = {
    parseAny(JSONSource.fromString(s))
  }

  /**
    * Parse any json values including null
    */
  def parseAny(s: Array[Byte]): JSONValue = {
    parseAny(JSONSource.fromBytes(s))
  }

  /**
    * Parse any json values including null
    */
  def parseAny(s: JSONSource): JSONValue = {
    val b = new JSONValueBuilder().singleContext(s, 0)
    JSONScanner.scanAny(s, b)
  }

  sealed trait JSONValue {
    override def toString: String = toJSON
    def toJSON: String
    def append(cb: CharBuilder): Unit = cb.append(toJSON)
  }

  final case object JSONNull extends JSONValue {
    override val toJSON: String = "null"
  }

  final case class JSONBoolean(v: Boolean) extends JSONValue {
    override val toJSON: String = if (v) "true" else "false"
  }

  val JSONTrue  = JSONBoolean(true)
  val JSONFalse = JSONBoolean(false)

  trait JSONNumber extends JSONValue
  final case class JSONDouble(v: Double) extends JSONNumber {
    override def toJSON: String = v.toString
  }
  final case class JSONLong(v: Long) extends JSONNumber {
    override def toJSON: String = v.toString
  }
  final case class JSONString(v: String) extends JSONValue {
    override def toString: String = v
    override def append(cb: CharBuilder): Unit = {
      cb.append("\"")
      cb.append(quoteJSONString(v))
      cb.append("\"")
    }
    override def toJSON: String = {
      val s = new CharBuilder()
      s.append("\"")
      s.append(quoteJSONString(v))
      s.append("\"")
      s.getAndReset
    }
  }

  final case class JSONObject(v: Seq[(String, JSONValue)]) extends JSONValue {
    override def toJSON: String = {
      val cb = new CharBuilder
      append(cb)
      cb.getAndReset
    }
    override def append(cb: CharBuilder): Unit = {
      cb.append("{")
      v.foreach {
        case (k, v: JSONValue) =>
          cb.append("\"")
          cb.append(quoteJSONString(k))
          cb.append("\":")
          v.append(cb)
          cb.append(",")
      }
      if (v.nonEmpty) cb.removeLast()
      cb.append("}")
    }
    def get(name: String): Option[JSONValue] = {
      v.collectFirst {
        case (key, value) if key == name =>
          value
      }
    }
  }
  final case class JSONArray(v: Seq[JSONValue]) extends JSONValue {
    override def toJSON: String = {
      val cb = new CharBuilder
      append(cb)
      cb.getAndReset
    }
    override def append(cb: CharBuilder): Unit = {
      cb.append("[")
      v.foreach { x =>
        x.append(cb)
        cb.append(",")
      }
      if (v.nonEmpty) cb.removeLast()
      cb.append("]")
    }
    def apply(i: Int): JSONValue = {
      v.apply(i)
    }
  }

  /**
    * This function can be used to properly quote Strings
    * for JSON output.
    */
  def quoteJSONString(s: String): String = {
    s.map {
      case '"'  => "\\\""
      case '\\' => "\\\\"
//      case '/'  => "\\/" We don't need to escape forward slashes
      case '\b' => "\\b"
      case '\f' => "\\f"
      case '\n' => "\\n"
      case '\r' => "\\r"
      case '\t' => "\\t"
      /* We'll unicode escape any control characters. These include:
       * 0x0 -> 0x1f  : ASCII Control (C0 Control Codes)
       * 0x7f         : ASCII DELETE
       * 0x80 -> 0x9f : C1 Control Codes
       *
       * Per RFC4627, section 2.5, we're not technically required to
       * encode the C1 codes, but we do to be safe.
       */
      case c if ((c >= '\u0000' && c <= '\u001f') || (c >= '\u007f' && c <= '\u009f')) => "\\u%04x".format(c.toInt)
      case c                                                                           => c
    }.mkString
  }
}
