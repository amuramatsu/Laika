/*
 * Copyright 2013-2016 the original author or authors.
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

package laika.parse.rst

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers

import laika.parse.helper.DefaultParserHelpers
import laika.parse.helper.ParseResultHelpers
import laika.tree.Elements.Span
import laika.tree.helper.ModelBuilder
import laika.parse.rst.Elements._
import laika.tree.Elements._
     
class InlineParsersSpec extends FlatSpec 
                        with Matchers 
                        with InlineParsers 
                        with ParseResultHelpers 
                        with DefaultParserHelpers[List[Span]] 
                        with ModelBuilder {

  
  val defaultParser: Parser[List[Span]] = spans(any,spanParsers) 
  
  def subst (name: String) = SubstitutionReference(name)
  
  
  def pLinkRef (id: String, text: String): LinkReference = linkRef(txt(text)).id(id).source(s"`$text`_")

  def pLinkRef (id: String): LinkReference = pLinkRef(id,id)
  
  def anonPLinkRef (text: String): LinkReference = linkRef(txt(text)).source(s"`$text`__")
  
  def linkRef (id: String): LinkReference = linkRef(txt(id)).id(id).source(id+"_")
  
  def anonLinkRef (text: String): LinkReference = linkRef(txt(text)).source(text+"__")
  
  
  
  "The text parser" should "parse content without any markup as plain text" in {
    Parsing ("some text") should produce (spans(txt("some text")))
  }
  
  
  "The markup recognition rules" should "recognize markup surrounded by whitespace" in {
    Parsing ("some |replaced| text") should produce (spans(txt("some "), subst("replaced"), txt(" text")))
  }

  it should "recognize markup surrounded by punctuation" in {
    Parsing ("some (|replaced|) text") should produce (spans(txt("some ("), subst("replaced"), txt(") text")))
  }
  
  it should "recognize markup at the start of the input" in {
    Parsing ("|replaced| text") should produce (spans(subst("replaced"), txt(" text")))
  }
  
  it should "recognize markup at the end of the input" in {
    Parsing ("text |replaced|") should produce (spans(txt("text "), subst("replaced")))
  }
  
  it should "recognize markup at the end and the end of the input" in {
    Parsing ("|replaced|") should produce (spans(subst("replaced")))
  }
  
  it should "ignore markup surrounded by characters (rules 1 and 4)" in {
    Parsing ("some|replaced|text") should produce (spans(txt("some|replaced|text")))
  }
  
  it should "ignore markup with a space after the start character (rule 2)" in {
    Parsing ("some | replaced| text") should produce (spans(txt("some | replaced| text")))
  }
  
  it should "ignore markup with a space before the end character (rule 3)" in {
    Parsing ("some |replaced | text") should produce (spans(txt("some |replaced | text")))
  }
  
  it should "ignore markup end characters immediately following the start character (rule 6)" in {
    Parsing ("some || text") should produce (spans(txt("some || text")))
  }
  
  it should "ignore markup characters between matching punctuation characters (rule 5)" in {
    Parsing ("some (|)replaced| text") should produce (spans(txt("some (|)replaced| text")))
  }
  
  it should "recognize markup when preceding and following characters are escaped" in {
    Parsing ("""some\ |replaced|\ text""") should produce (spans(txt("some"), subst("replaced"), txt("text")))
  }
  
  
  
  "The em parser" should "parse content enclosed in *" in {
    Parsing ("some *text* here") should produce (spans(txt("some "),em(txt("text")),txt(" here")))
  }
  
  it should "ignore an * character when it is not matched by a second *" in {
    Parsing ("some *text here") should produce (spans(txt("some *text here")))
  }
  
  it should "ignore markup for nested spans" in {
    Parsing ("some *nested ``code`` span* here") should produce (spans(txt("some "),em(txt("nested ``code`` span")),txt(" here")))
  }
  
  it should "ignore markup for emphasis when strong failed at the same position based on the markup recognition rules" in {
    Parsing ("a**b O(N**2)") should produce (spans(txt("a**b O(N**2)")))
  }
    
  
  
  "The strong parser" should "parse content enclosed in **" in {
    Parsing ("some **text** here") should produce (spans(txt("some "),str(txt("text")),txt(" here")))
  }
  
  it should "ignore a ** sequence when it is not matched by a second **" in {
    Parsing ("some **text here") should produce (spans(txt("some **text here")))
  }
  
  
  
  "The inline literal parser" should "parse content enclosed in ``" in {
    Parsing ("some ``text`` here") should produce (spans(txt("some "),lit("text"),txt(" here")))
  }
  
  it should "ignore a ` character when it is not matched by a second `" in {
    Parsing ("some ``text here") should produce (spans(txt("some ``text here")))
  }
  
  it should "not treat a single ` as markup when the code span is enclosed in double ``" in {
    Parsing ("some ``text`text`` here") should produce (spans(txt("some "),lit("text`text"),txt(" here")))
  }
  
  
  
  "The substitution reference" should "parse content enclosed in |" in {
    Parsing ("some |replaced| text") should produce (spans(txt("some "), subst("replaced"), txt(" text")))
  }
  
  it should "ignore a | character when it is not matched by a second |" in {
    Parsing ("some |text here") should produce (spans(txt("some |text here")))
  }
  
  
  
  "The internal link target parser" should "parse content enclosed in _` and `" in {
    Parsing ("some _`Text` here") should produce (spans(txt("some "), Text("Text", Id("text") + Styles("target")), txt(" here")))
  }
  
  
  
  "The interpreted text parser" should "parse content enclosed in ` with implicit default role" in {
    Parsing ("some `text` here") should produce (spans(txt("some "), InterpretedText(defaultTextRole,"text","`text`"), txt(" here")))
  }
  
  it should "parse content enclosed in ` with role prefix" in {
    Parsing ("some :role:`text` here") should produce (spans(txt("some "), InterpretedText("role","text",":role:`text`"), txt(" here")))
  }
  
  it should "parse content enclosed in ` with role suffix" in {
    Parsing ("some `text`:role: here") should produce (spans(txt("some "), InterpretedText("role","text","`text`:role:"), txt(" here")))
  }
  
  it should "parse content enclosed in ` but ignore illegal role prefix" in {
    Parsing ("some :#*#:`text` here") should produce (spans(txt("some :#*#:"), InterpretedText(defaultTextRole,"text","`text`"), txt(" here")))
  }
  
  
  
  "The citation reference parser" should "parse content enclosed between [ and ]_" in {
    Parsing ("some [text]_ here") should produce (spans(txt("some "), citRef("text"), txt(" here")))
  }
  
  
  
  "The footnote reference parser" should "parse content enclosed between [ and ]_ with autonumber label" in {
    Parsing ("some [#]_ here") should produce (spans(txt("some "), fnRef(Autonumber), txt(" here")))
  }
  
  it should "parse content enclosed between [ and ]_ with autosymbol label" in {
    Parsing ("some [*]_ here") should produce (spans(txt("some "), fnRef(Autosymbol), txt(" here")))
  }
  
  it should "parse content enclosed between [ and ]_ with an autonumber named label" in {
    Parsing ("some [#foo]_ here") should produce (spans(txt("some "), fnRef(AutonumberLabel("foo")), txt(" here")))
  }
  
  it should "parse content enclosed between [ and ]_ with a numeric label" in {
    Parsing ("some [17]_ here") should produce (spans(txt("some "), fnRef(NumericLabel(17)), txt(" here")))
  }
  
  
  
  "The link reference parser" should "parse a phrase link without url" in {
    Parsing ("some `link`_ here") should produce (spans(txt("some "), pLinkRef("link"), txt(" here")))
  }
  
  it should "parse a phrase link with text and url" in {
    Parsing ("some `link<http://foo.com>`_ here") should produce (spans(txt("some "), 
        SpanSequence(List(link(txt("link")).url("http://foo.com"), ExternalLinkDefinition("link", "http://foo.com"))), txt(" here")))
  }
  
  it should "parse a phrase link with only an url" in {
    Parsing ("some `<http://foo.com>`_ here") should produce (spans(txt("some "), 
        SpanSequence(List(link(txt("http://foo.com")).url("http://foo.com"), ExternalLinkDefinition("http://foo.com", "http://foo.com"))), txt(" here")))
  }
  
  it should "remove whitespace from an url" in {
    val input = """some `<http://
      | foo.com>`_ here""".stripMargin
    Parsing (input) should produce (spans(txt("some "), 
        SpanSequence(List(link(txt("http://foo.com")).url("http://foo.com"), ExternalLinkDefinition("http://foo.com", "http://foo.com"))), txt(" here")))
  }
  
  it should "parse an anonymous phrase link without url" in {
    Parsing ("some `link`__ here") should produce (spans(txt("some "), anonPLinkRef("link"), txt(" here")))
  }
  
  it should "parse an anonymous phrase link with text and url" in {
    Parsing ("some `link<http://foo.com>`__ here") should produce (spans(txt("some "), 
        link(txt("link")).url("http://foo.com"), txt(" here")))
  }
  
  it should "parse an anonymous phrase link with only an url" in {
    Parsing ("some `<http://foo.com>`__ here") should produce (spans(txt("some "), 
        link(txt("http://foo.com")).url("http://foo.com"), txt(" here")))
  }
  
  it should "parse a named link reference" in {
    Parsing ("some link_ here") should produce (spans(txt("some link"), Reverse(4, linkRef("link"), txt("_")), txt(" here")))
  }
  
  it should "parse an anonymous link reference" in {
    Parsing ("some link__ here") should produce (spans(txt("some link"), Reverse(4, anonLinkRef("link"), txt("__")) , txt(" here")))
  }
  
  it should "normalize the id of a phrase link" in {
    val input = """some `strange
      | phrase   link`_ here""".stripMargin
    Parsing (input) should produce (spans(txt("some "), pLinkRef("strange phrase link","strange\n phrase   link"), txt(" here")))
  }
  
  
  "The standalone link parser" should "parse a http URI" in {
    val uri = "http://www.link.com"
    Parsing ("some http://www.link.com here") should produce (spans(txt("some http"), 
        Reverse(4, link(txt(uri)).url(uri), txt("://www.link.com")), txt(" here")))
  }
  
  it should "parse a https URI" in {
    val uri = "https://www.link.com"
    Parsing ("some https://www.link.com here") should produce (spans(txt("some https"), 
        Reverse(5, link(txt(uri)).url(uri), txt("://www.link.com")), txt(" here")))
  }
  
  it should "parse an email address" in {
    val email = "user@domain.com"
    Parsing ("some user@domain.com here") should produce (spans(txt("some user"), 
        Reverse(4, link(txt(email)).url("mailto:"+email), txt("@domain.com")), txt(" here")))
  }
  
  it should "parse a http URI without trailing punctuation" in {
    val uri = "http://www.link.com"
    Parsing ("some http://www.link.com. here") should produce (spans(txt("some http"), 
        Reverse(4, link(txt(uri)).url(uri), txt("://www.link.com")), txt(". here")))
  }
  
  it should "parse an email address without surrounding punctuation" in {
    val email = "user@domain.com"
    Parsing ("some {user@domain.com} here") should produce (spans(txt("some {user"), 
        Reverse(4, link(txt(email)).url("mailto:"+email), txt("@domain.com")), txt("} here")))
  }

  it should "parse a http URI containing IP address, starts with 3digit" in {
    val uri = "http://127.0.0.1"
    Parsing ("some http://127.0.0.1/. here") should produce (spans(txt("some http"), 
        Reverse(4, link(txt(uri)).url(uri), txt("://127.0.0.1")), txt("/. here")))
  }

  it should "parse a http URI containing IP address, starts with 2 digit" in {
    val uri = "http://10.20.30.40"
    Parsing ("some http://10.20.30.40/. here") should produce (spans(txt("some http"), 
        Reverse(4, link(txt(uri)).url(uri), txt("://10.20.30.40")), txt("/. here")))
  }

  it should "parse a http URI containing IP address, starts with 1 digit" in {
    val uri = "http://1.2.3.4"
    Parsing ("some http://1.2.3.4/. here") should produce (spans(txt("some http"), 
        Reverse(4, link(txt(uri)).url(uri), txt("://1.2.3.4")), txt("/. here")))
  }

  "A backslash " should "cause a following character not to be treated as markup" in {
    Parsing ("""some \*text* here""") should produce (spans(txt("some *text* here")))
  }
  
  
}
