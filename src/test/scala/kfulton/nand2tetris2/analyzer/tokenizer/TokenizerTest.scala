package kfulton.nand2tetris2.analyzer.tokenizer

import kfulton.nand2tetris2.analyzer.tokenizer.tokens.{Class, IntToken, KeywordToken, LeftParen, SymbolToken, TokenizerError}
import org.scalatest.{FlatSpec, Matchers}

class TokenizerTest extends FlatSpec with Matchers {
  val tokenizer = new Tokenizer

  //"class Pointe { method int getx() { return x;} }"
  "advance" should "find the next token and return token type" in {
    val program = "class {" #:: "return }" #:: Stream.empty
    val tokens = tokenizer.advance(program)._1
    tokens.size shouldBe 4
    tokens.head.isLeft shouldBe true
    tokens.head match {
      case Left(key) => key shouldBe KeywordToken(Class)
      case _ => "Should not hit this case"
    }
  }

  it should "handle single line comments" in {
    val program = "class {" #:: "//This is a comment" #:: "return }" #:: Stream.empty
    val tokens = tokenizer.advance(program)._1
    tokens.size shouldBe 4
  }

  it should "handle multiline comments" in {
    val program = "class {" #:: "/*This" #:: "class should not count */ return }" #:: Stream.empty
    val tokens = tokenizer.advance(program)._1
    tokens.size shouldBe 4
  }

  it should "return a TokenizerError if it cannot find a valid token" in {
    pending
  }

  "isIgnored" should "ignore non-tokens like white space and comments" in {
    val comment = "//This is just a comment"
    tokenizer.isSignalLineIgnored(comment) shouldBe true
  }

  it should "ignore white space" in {
    val whiteSpace = "  "
    tokenizer.isSignalLineIgnored(whiteSpace) shouldBe true
  }

  "tokenType" should "return the correct symbol token type" in {
    tokenizer.tokenType("(") shouldBe Left(SymbolToken(LeftParen))
  }

  it should "return the correct keyword token type" in {
    tokenizer.tokenType("class") shouldBe Left(KeywordToken(Class))
  }

  it should "return a tokenizerError when token is invalid" in {
    tokenizer.tokenType("%") shouldBe Right(TokenizerError("% is invalid token."))
  }

  it should "return the correct integer token type" in {
    tokenizer.tokenType("0") shouldBe Left(IntToken(0))
    tokenizer.tokenType("32767") shouldBe Left(IntToken(32767))
  }

  it should "fail if it is not in expected integer range" in {
    tokenizer.tokenType("-1") shouldBe Right(TokenizerError("Invalid integer: -1."))
    tokenizer.tokenType("32768") shouldBe Right(TokenizerError("Invalid integer: 32768."))
  }

  it should "return the correct string constant token type" in {
    pending
  }

  it should "return the correct identifier token type" in {
    pending
  }
}