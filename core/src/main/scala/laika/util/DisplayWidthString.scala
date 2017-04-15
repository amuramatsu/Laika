/*
 * Copyright 2017 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package laika.util

import com.ibm.icu.lang.{UCharacter, UProperty}

/** Calculate display width of String.
 *  This class takes care to surrogate pairs and East-Asian Character Width.
 * 
 *  @author MURAMATSU Atsushi <amura@tomato.sakura.ne.jp>
 */
class DisplayWidthString(string: String) {

  /** Split strings into codepoint array.
    */
  private def toCodePointArray(str: String) =
    (0 until str.codePointCount(0, str.length)) map {
      str.offsetByCodePoints(0, _)
    } map { str.codePointAt(_) }

  /** Check EastAsianWidth by icu4j
    */
  private def getCharDisplayWidth(codePoint: Int) =
    UCharacter.getIntPropertyValue(codePoint, UProperty.EAST_ASIAN_WIDTH) match {
      case UCharacter.EastAsianWidth.FULLWIDTH
         | UCharacter.EastAsianWidth.WIDE  => 2
      case _ => 1
    }

  /** Return lengthes of string at each codepoint
    */
  private def getLengthOfEachCodePoint(str: String) = {
    val charWidth = toCodePointArray(str).map{ getCharDisplayWidth(_) }
    (0 until charWidth.length).map{
      n => (charWidth take n).sum
    }
  }

  /** Return display width of string.
   */
  def displayWidth: Int =
    toCodePointArray(string).map{ getCharDisplayWidth(_) }.sum

  /** Take "num" width string
    */
  def takeDisplayWidth(num: Int): String = {
    getLengthOfEachCodePoint(string).indexWhere(_ >= num) match {
      case n: Int if n >= 0 => string take string.offsetByCodePoints(0, n)
      case _ => string
    }
  }

  /** Return Char from String at "num" display column
    */
  def charAtDisplayWidth(num: Int): Char = {
    getLengthOfEachCodePoint(string).indexWhere(_ >= num) match {
      case n: Int if n >= 0 => string charAt string.offsetByCodePoints(0, n)
      case _ => throw new IndexOutOfBoundsException()
    }
  }

  /** Return sub-sequence of CharSequence, count with display width
    */
  def subSequenceDisplayWidth(start: Int, end: Int): DisplayWidthCharSequence = {
    val str = new DisplayWidthString(string)
    val start_codepos = str.takeDisplayWidth(start).length
    val end_codepos = str.takeDisplayWidth(end).length
    new DisplayWidthCharSequence(string.subSequence(start_codepos, end_codepos))
  }
}

/** Calculate display width of CharSequence.
 *  This class takes care to surrogate pairs and East-Asian Character Width.
 * 
 *  @author MURAMATSU Atsushi <amura@tomato.sakura.ne.jp>
 */
class DisplayWidthCharSequence(chars: CharSequence) {
  /** Return Char from CharSequence at "num" display column
    */
  def charAtDisplayWidth(index: Int): Char =
    (new DisplayWidthString(chars.toString)).charAtDisplayWidth(index)

  /** Return display width of CharSequence
    */
  def displayWidth: Int =
    (new DisplayWidthString(chars.toString)).displayWidth

  /** Return sub-sequence of CharSequence, count with display width
    */
  def subSequenceDisplayWidth(start: Int, end: Int): DisplayWidthCharSequence =
    (new DisplayWidthString(chars.toString)).subSequenceDisplayWidth(start, end)

  override def toString = chars.toString
}
