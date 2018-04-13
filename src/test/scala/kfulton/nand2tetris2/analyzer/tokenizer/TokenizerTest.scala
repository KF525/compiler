package kfulton.nand2tetris2.analyzer.tokenizer

import org.scalatest.{FlatSpec, Matchers}

class TokenizerTest extends FlatSpec with Matchers {
  val tokenizer = new Tokenizer

  "tokenType" should "return the correct symbol token type" in {
    tokenizer.tokenType("(") shouldBe Left(Symbol('('))
  }

  it should "return the correct keyword token type" in {
    tokenizer.tokenType("class") shouldBe Left(Keyword(Class))
  }

  it should "return a tokenizerError when token is invalid" in {
    tokenizer.tokenType("&") shouldBe Right(TokenizerError("& is invalid token."))
  }
}

/*
terminal
keword symbol integer constant string constant identifier
 */