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

/** Return display width of Character.
 * 
 *  @author MURAMATSU Atsushi <amura@tomato.sakura.ne.jp>
 */
object CharacterDisplayWidth {
  /** Check EastAsianWidth by icu4j
    */
  def getCharDisplayWidth(codePoint: Int): Int =
    UCharacter.getIntPropertyValue(codePoint, UProperty.EAST_ASIAN_WIDTH) match {
      case UCharacter.EastAsianWidth.FULLWIDTH
         | UCharacter.EastAsianWidth.WIDE  => 2
      case _ => 1
    }

  def getCharDisplayWidth(char: Char): Int = getCharDisplayWidth(char: Int)
}

/** Calculate display width of String.
 *  This class takes care to surrogate pairs and East-Asian Character Width.
 * 
 *  @author MURAMATSU Atsushi <amura@tomato.sakura.ne.jp>
 */
class DisplayWidthString(string: String) {
  import CharacterDisplayWidth._

  /** Split strings into codepoint array.
    */
  private lazy val codePointArray: Array[Int] =
    ((0 until string.codePointCount(0, string.length)) map {
      string.offsetByCodePoints(0, _)
    } map { string.codePointAt(_) }).toArray

  /** Return lengthes of string at each codepoint
    */
  private lazy val lengthOfEachCodePoint: Seq[Int] = {
    if (string.length == 0) {
      Seq[Int]()
    }
    else {
      import scala.collection.mutable.ArraySeq
      val charWidth = codePointArray map { getCharDisplayWidth(_) }
      val result = new ArraySeq[Int](charWidth.length)
      result(0) = 0
      for (i <- 0 until charWidth.length-1)
        result(i+1) = result(i) + charWidth(i)
      result
    }
  }

  /** Split strings to each column
    */
  lazy val stringsAtEachColumn: Seq[String] = {
    (0 until codePointArray.length) flatMap { offset =>
      getCharDisplayWidth(codePointArray(offset)) match {
        case 1 => Seq(new String(codePointArray, offset, 1))
        case 2 => Seq(new String(codePointArray, offset, 1), "")
      }
    }
  }

  /** Return display width of string.
   */
  lazy val displayWidth: Int =
    codePointArray.map{ getCharDisplayWidth(_) }.sum

  /** Take "num" width string
    */
  def takeDisplayWidth(num: Int): String = {
    lengthOfEachCodePoint.indexWhere(_ >= num) match {
      case n: Int if n >= 0 => string take string.offsetByCodePoints(0, n)
      case _ => string
    }
  }

  /** Return Char from String at "num" display column
    */
  def charsAtDisplayWidth(num: Int): Seq[Char] =
    stringsAtEachColumn(num).toCharArray

  /** Return sub-sequence of CharSequence, count with display width
    */
  def subSequenceDisplayWidth(start: Int, end: Int) =
    new DisplayWidthCharSequence((start until end) map { stringsAtEachColumn(_) } mkString "")
}

/** Calculate display width of CharSequence.
 *  This class takes care to surrogate pairs and East-Asian Character Width.
 * 
 *  @author MURAMATSU Atsushi <amura@tomato.sakura.ne.jp>
 */
class DisplayWidthCharSequence(chars: CharSequence) {
  private lazy val string = new DisplayWidthString(chars.toString)

  /** Return Char from CharSequence at "num" display column
    */
  def charsAtDisplayWidth(index: Int): Seq[Char] =
    string.charsAtDisplayWidth(index)

  /** Return display width of CharSequence
    */
  def displayWidth: Int = string.displayWidth

  /** Split strings to each column
    */
  lazy val stringsAtEachColumn: Seq[String] = string.stringsAtEachColumn

  /** Return sub-sequence of CharSequence, count with display width
    */
  def subSequenceDisplayWidth(start: Int, end: Int): DisplayWidthCharSequence =
    string.subSequenceDisplayWidth(start, end)

  override def toString: String = chars.toString
}
