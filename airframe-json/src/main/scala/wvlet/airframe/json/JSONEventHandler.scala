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

import scala.collection.mutable._

trait JSONHandler[Expr] {
  def singleContext(): JSONContext[Expr]
  def objectContext()(implicit buffer: ArrayBuffer[(String, Expr)]): JSONContext[Expr]
  def arrayContext()(implicit buffer: ArrayBuffer[Expr]): JSONContext[Expr]
}

/**
  * A facade to build json ASTs while scanning json with JSONScanner
  *
  * @tparam Expr
  */
trait JSONContext[Expr] extends JSONHandler[Expr] {
  def result: Expr
  def isObjectContext: Boolean
  private[json] final def endScannerState: Int = {
    if (isObjectContext) {
      JSONScanner.OBJECT_END
    } else {
      JSONScanner.ARRAY_END
    }
  }

  def add(v: Expr): Unit
  def closeContext(): Unit
  def addNull(s: JSONSource, start: Int, end: Int): Unit
  def addString(s: JSONSource, start: Int, end: Int): Unit
  def addUnescapedString(s: String): Unit
  def addNumber(s: String): Unit
  def addBoolean(s: JSONSource, v: Boolean, start: Int, end: Int): Unit
}
