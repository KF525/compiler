package kfulton.nand2tetris2.analyzer.printer

import kfulton.nand2tetris2.analyzer.tokenizer
import kfulton.nand2tetris2.analyzer.tokenizer.{tokens, _}
import kfulton.nand2tetris2.analyzer.tokenizer.tokens.{IdentifierToken, KeywordToken, Minus, Plus, StringToken, SymbolToken, Token}
import org.scalatest.{FlatSpec, Matchers}

import scala.xml.Node

class XMLPrinterTest extends FlatSpec with Matchers {
  val xmlPrinter = new XMLPrinter

  "printToken" should "return the correct xml" in {
    //xmlPrinter.printToken(IntToken(3)) shouldBe <integerConstant>3</integerConstant>
    xmlPrinter.printToken(StringToken("string")) shouldBe <stringConstant>string</stringConstant>
    xmlPrinter.printToken(SymbolToken(Plus)) shouldBe <symbol>+</symbol>
    xmlPrinter.printToken(KeywordToken(tokens.Class)) shouldBe <keyword>class</keyword>
    xmlPrinter.printToken(IdentifierToken("x")) shouldBe <identifier>x</identifier>
  }

  "printTokens" should "return a list of xml" in {
    val token1: Token = SymbolToken(Minus)
    val token2: Token = StringToken("string")
    val token3: Token = tokenizer.tokens.KeywordToken(tokenizer.tokens.Class)
    val tokens: Stream[Token] = token1 #:: token2 #:: token3 #:: Stream.empty
    val node: Node = <tokens><symbol>-</symbol><stringConstant>string</stringConstant><keyword>class</keyword></tokens>;
    xmlPrinter.printTokens(tokens) shouldBe node
  }

  "saveXML" should "return a file" in {
    val xml: Node = <tokens><stringConstant>string1</stringConstant><integerConstant>3</integerConstant><stringConstant>string2</stringConstant></tokens>
    xmlPrinter.saveXML("fileName", xml)
  }
}
