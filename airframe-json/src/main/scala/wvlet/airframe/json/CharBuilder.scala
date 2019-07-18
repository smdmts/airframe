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

private[json] class CharBuilder {
  @inline final def INITIALSIZE = 1024

  private var cs       = new Array[Char](INITIALSIZE)
  private var capacity = INITIALSIZE
  private var len      = 0

  def reset(): Unit = {
    len = 0
  }

  def getAndReset: String = {
    val result = new String(cs, 0, len)
    reset()
    result
  }

  def resizeIfNecessary(goal: Int): Unit = {
    if (goal <= capacity) return
    var cap = capacity
    while (goal > cap && cap > 0) cap *= 2
    if (cap > capacity) {
      val ncs = new Array[Char](cap)
      System.arraycopy(cs, 0, ncs, 0, capacity)
      cs = ncs
      capacity = cap
    }
  }

  def removeLast(): Unit = {
    cs(len) = ' '
    len -= 1
  }

  def last():Char = {
    cs(len - 1)
  }

  def append(b: Byte): Unit = {
    append(b.toChar)
  }

  def append(c: Char): Unit = {
    val tlen = len + 1
    resizeIfNecessary(tlen)
    cs(len) = c
    len = tlen
  }

  def append(s: String): Unit = {
    val tlen = len + s.length
    resizeIfNecessary(tlen)
    var index = 0
    (len until tlen).foreach { i =>
      cs(i) = s(index)
      index += 1
    }
    len = tlen
  }
}
