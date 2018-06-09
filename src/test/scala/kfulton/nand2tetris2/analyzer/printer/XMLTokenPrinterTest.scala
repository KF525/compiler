package kfulton.nand2tetris2.analyzer.printer

import kfulton.nand2tetris2.analyzer.tokenizer.tokens._
import org.scalatest.{FlatSpec, Matchers}

class XMLTokenPrinterTest extends FlatSpec with Matchers {
  val xmlPrinter = new XMLTokenPrinter

  "printTokens" should "return the correct xml" in {
    val tokens = List(KeywordToken(If), SymbolToken(LeftParen), IdentifierToken("x"),
      SymbolToken(LessThan), IntToken(153), SymbolToken(RightParen),
      SymbolToken(LeftCurlyBracket), KeywordToken(Let), IdentifierToken("city"), SymbolToken(Equal),
      StringToken("Paris"), SymbolToken(SemiColon), SymbolToken(RightParen))
    val result = xmlPrinter.printTokens(tokens, "testFile")
    result shouldBe ()
  }
}
