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

import wvlet.airframe.AirframeSpec
import wvlet.airframe.json.JSON._

/**
  *
  */
class JSONParserTest extends AirframeSpec {

  def parse(s: String): JSONValue = {
    val v = JSON.parse(JSONSource.fromString(s))
    debug(s"parse ${s}: ${v}")
    v
  }

  "JSONParser" should {
    "parser json string" in {
      parse("{}")
      parse("""{"id":1, "name":"leo", "value":0.1, "num":10000000000000000000000000}""")
    }

    "parser json multi-byte string" in {
      parse("{}")
      parse(
        """{"id":1, "name":"れお👌", "value":0.1, "num":10000000000000000000000000}""".stripMargin)
    }

    "parse large array of objects" in {
      val json = (for (_ <- 0 to 10000) yield "{}").mkString("[", ",", "]")
      parse(json)
    }

    "parse any json values" in {
      val v = JSON.parseAny("null")
      v shouldBe JSONNull
      JSON.parseAny("1") shouldBe JSONLong(1L)
      JSON.parseAny("1.23") shouldBe JSONDouble(1.23)
      JSON.parseAny("[]") shouldBe JSONArray(IndexedSeq.empty)
      JSON.parseAny("[1, 2]") shouldBe JSONArray(IndexedSeq(JSONLong(1L), JSONLong(2L)))
      JSON.parseAny("""{"id":1}""") shouldBe JSONObject(Seq("id" -> JSONLong(1L)))
    }
  }
}
