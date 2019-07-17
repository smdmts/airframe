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

import wvlet.airframe.json.JSON._
import wvlet.log.LogSupport

import scala.collection.mutable

class JSONValueBuilder extends JSONContext[JSONValue] with LogSupport { self =>

  override def result: JSONValue                           = null
  override def isObjectContext: Boolean                    = false
  override def closeContext(s: JSONSource, end: Int): Unit = {}
  def add(v: JSONValue): Unit                              = {}

  override def singleContext(s: JSONSource, start: Int): JSONContext[JSONValue] =
    new JSONValueBuilder {
      private[this] var holder: JSONValue                      = _
      override def isObjectContext                             = false
      override def closeContext(s: JSONSource, end: Int): Unit = {}
      override def add(v: JSONValue): Unit = {
        holder = v
      }
      override def result: JSONValue = holder
    }

  override def objectContext(s: JSONSource, start: Int): JSONContext[JSONValue] =
    new JSONValueBuilder {
      private[this] var key: String = _
      private[this] val list        = mutable.ArrayBuffer.empty[(String, JSONValue)]
      override def closeContext(s: JSONSource, end: Int): Unit = {
        self.add(result)
      }
      override def isObjectContext: Boolean = true
      override def add(v: JSONValue): Unit = {
        if (key == null) {
          key = v.toString
        } else {
          list.append(key -> v)
          key = null
        }
      }
      override def result: JSONValue = {
        JSONObject(list)
      }
    }

  override def arrayContext(s: JSONSource, start: Int): JSONContext[JSONValue] =
    new JSONValueBuilder {
      private[this] val list                = mutable.ArrayBuffer.empty[JSONValue]
      override def isObjectContext: Boolean = false
      override def closeContext(s: JSONSource, end: Int): Unit = {
        self.add(result)
      }
      override def add(v: JSONValue): Unit = {
        list.append(v)
      }
      override def result: JSONValue = {
        JSONArray(list)
      }
    }

  override def addNull(s: JSONSource, start: Int, end: Int): Unit = {
    add(JSONNull)
  }
  override def addString(s: JSONSource, start: Int, end: Int): Unit = {
    add(JSONString(s.substring(start, end)))
  }
  override def addUnescapedString(s: String): Unit = {
    add(JSONString(s))
  }
  override def addNumber(s: JSONSource, start: Int, end: Int, dotIndex: Int, expIndex: Int): Unit = {
    def parseLongUnsafe(cs: CharSequence): Long = {
      // we store the inverse of the positive sum, to ensure we don't
      // incorrectly overflow on Long.MinValue. for positive numbers
      // this inverse sum will be inverted before being returned.
      var inverseSum: Long  = 0L
      var inverseSign: Long = -1L
      var i: Int            = 0
      if (cs.charAt(0) == '-') {
        inverseSign = 1L
        i = 1
      }
      val len = cs.length
      while (i < len) {
        inverseSum = inverseSum * 10L - (cs.charAt(i).toInt - 48)
        i += 1
      }
      inverseSum * inverseSign
    }

    lazy val v = s.substring(start, end)
    lazy val num: JSONNumber = if (dotIndex == -1 && expIndex == -1) {
      JSONLong(parseLongUnsafe(v))
    } else {
      JSONDouble(java.lang.Double.parseDouble(v))
    }
    add(num)
  }

  override def addBoolean(s: JSONSource, v: Boolean, start: Int, end: Int): Unit = {
    val b = if (v) JSONTrue else JSONFalse
    add(b)
  }
}
