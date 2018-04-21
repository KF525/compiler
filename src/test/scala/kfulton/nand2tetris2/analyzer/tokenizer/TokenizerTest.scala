package kfulton.nand2tetris2.analyzer.tokenizer

import kfulton.nand2tetris2.analyzer.tokenizer.tokens._
import org.scalatest.{FlatSpec, Matchers}

class TokenizerTest extends FlatSpec with Matchers {
  val tokenizer = new Tokenizer

  "advance" should "find the next token and return token type" in {
    val program = "class {" #:: "return }" #:: Stream.empty
    val tokens = tokenizer.advance(program)
    tokens.size shouldBe 4
    tokens.head.isLeft shouldBe true
    tokens.head match {
      case Left(k) => k shouldBe KeywordToken(Class)
      case _ => "Should not hit this case"
    }
  }

  it should "handle multiple lines with different token types" in {
    val program = "class Point {" #:: "method int getx() {" #:: "return x;}}" #:: Stream.empty
    val tokens = tokenizer.advance(program)
    tokens.head shouldBe Left(KeywordToken(Class))
    tokens.tail.head shouldBe Left(IdentifierToken("Point"))
    tokens.tail.tail.head shouldBe Left(SymbolToken(LeftCurlyBracket))
    tokens.tail.tail.tail.head shouldBe Left(KeywordToken(Method))
    tokens.tail.tail.tail.tail.head shouldBe Left(IdentifierToken("int"))
    tokens.tail.tail.tail.tail.tail.head shouldBe Left(IdentifierToken("getx"))
    tokens.tail.tail.tail.tail.tail.tail.head shouldBe Left(SymbolToken(LeftParen))
    tokens.tail.tail.tail.tail.tail.tail.tail.head shouldBe Left(SymbolToken(RightParen))
    tokens.tail.tail.tail.tail.tail.tail.tail.tail.head shouldBe Left(SymbolToken(LeftCurlyBracket))
    tokens.tail.tail.tail.tail.tail.tail.tail.tail.tail.head shouldBe Left(KeywordToken(Return))
    tokens.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head shouldBe Left(IdentifierToken("x"))
    tokens.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head shouldBe Left(SymbolToken(SemiColon))
    tokens.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.tail.head shouldBe Left(SymbolToken(RightCurlyBracket))
    tokens.size shouldBe 14
  }

  it should "handle single line comments" in {
    val program = "class {" #:: "// This is a comment" #:: "return }" #:: Stream.empty
    val tokens = tokenizer.advance(program)
    tokens.size shouldBe 4
  }

  it should "handle multiline comments" in {
    val program = "class { " #:: "/* This" #:: "class should not count */ return } " #:: Stream.empty
    val tokens = tokenizer.advance(program)
    tokens.size shouldBe 4
  }

  it should "handle multiple white space" in {
    val program = "  class" #:: Stream.empty
    val tokens = tokenizer.advance(program)
    tokens.size shouldBe 1
  }

  //TODO: How do we want to fail? Right now as soon as we hit an invalid token, nothing else gets written.
  it should "return a TokenizerError if it cannot find a valid token" in {
    val programWithInvalid = "class %" #:: Stream.empty
    val tokens = tokenizer.advance(programWithInvalid)
    tokens.size shouldBe 1
    tokens.head shouldBe Left(KeywordToken(Class))
  }

  "isIgnored" should "ignore non-tokens like white space and comments" in {
    val comment = "//This is just a comment"
    tokenizer.isSignalLineIgnored(comment) shouldBe true
  }

  it should "ignore white space" in {
    val whiteSpace = " "
    tokenizer.isSignalSpaceIgnored(whiteSpace) shouldBe true
  }

  "tokenType" should "return the correct symbol token type" in {
    tokenizer.tokenType("(") shouldBe Left(SymbolToken(LeftParen))
  }

  it should "return the correct keyword token type" in {
    tokenizer.tokenType("class") shouldBe Left(KeywordToken(Class))
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
    tokenizer.tokenType("\"string\"") shouldBe Left(StringToken("string"))
  }

  it should "return the correct identifier token type" in {
    tokenizer.tokenType("id_ex0") shouldBe Left(IdentifierToken("id_ex0"))
  }

  it should "return an identifier even if a keyword is present" in {
    tokenizer.tokenType("class_is_identifier") shouldBe Left(IdentifierToken("class_is_identifier"))
  }

  it should "return a tokenizerError when token is invalid" in {
    tokenizer.tokenType("%") shouldBe Right(TokenizerError("% is invalid token."))
  }
}