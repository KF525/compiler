package kfulton.nand2tetris2.analyzer.parser

import cats.implicits._
import kfulton.nand2tetris2.analyzer.tokenizer.tokens._
import org.scalatest.{FlatSpec, Matchers}

class JStatementsParserTest extends FlatSpec with Matchers {
  val parser = JStatementsParser

  "parseJStatement" should "return new state with JReturn" in {
    val tokens = List(KeywordToken(Return), IntToken(4), SymbolToken(SemiColon), IntToken(5), SymbolToken(Comma))

    val result = parser.parseJStatement().run(tokens)
    result shouldBe Right(List(IntToken(5), SymbolToken(Comma)), JReturnStatement(JExpression(JIntegerTerm(4), List())))
  }

  it should "return new state with JLet" in {
    val tokens = List(
      KeywordToken(Let),
      IdentifierToken("id"),
      SymbolToken(LeftSquareBracket),
      IntToken(3),
      SymbolToken(RightSquareBracket),
      SymbolToken(Equal),
      IntToken(4),
      SymbolToken(SemiColon)
    )

    val result = parser.parseJStatement().run(tokens)
    result shouldBe Right(List(), JLetStatement(JName("id"), Some(JExpression(JIntegerTerm(3), List())),
      JExpression(JIntegerTerm(4), List())))
  }

  it should "handle a JLet without additional expression" in {
    val tokens = List(
      KeywordToken(Let),
      IdentifierToken("id"),
      SymbolToken(Equal),
      IntToken(4),
      SymbolToken(SemiColon)
    )

    val result = parser.parseJStatement().run(tokens)
    result shouldBe Right(List(), JLetStatement(JName("id"), None, JExpression(JIntegerTerm(4), List())))
  }

  "parseJIf" should "return new state with JIf" in {
    val tokens = List(KeywordToken(If), SymbolToken(LeftParen), IntToken(3),
      SymbolToken(RightParen), SymbolToken(LeftCurlyBracket), KeywordToken(Return), IntToken(3),
      SymbolToken(SemiColon), SymbolToken(RightCurlyBracket), KeywordToken(Else), SymbolToken(LeftCurlyBracket),
      KeywordToken(Return), IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket)
    )
    val result = parser.parseJStatement().run(tokens)
    result shouldBe
      Right(List(), JIfStatement(
        JExpression(JIntegerTerm(3), List()),
        JStatements(List(JReturnStatement(JExpression(JIntegerTerm(3), List())))),
        Some(JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List())))))))
  }

  it should "handle expressions that do not include else" in {
    val tokens = List(KeywordToken(If), SymbolToken(LeftParen), IntToken(3),
      SymbolToken(RightParen), SymbolToken(LeftCurlyBracket), KeywordToken(Return), IntToken(3),
      SymbolToken(SemiColon), SymbolToken(RightCurlyBracket), KeywordToken(Let)
    )
    val result = parser.parseJStatement().run(tokens)
    result shouldBe
      Right(List(KeywordToken(Let)), JIfStatement(
        JExpression(JIntegerTerm(3), List()),
        JStatements(List(JReturnStatement(JExpression(JIntegerTerm(3), List())))),
        None))
  }

  "parseJWhile" should "return new state with JWhile" in {
    val tokens = List(KeywordToken(While), SymbolToken(LeftParen), IntToken(3),
      SymbolToken(RightParen), SymbolToken(LeftCurlyBracket),KeywordToken(Return), IntToken(4),
      SymbolToken(SemiColon), SymbolToken(RightCurlyBracket), IdentifierToken("test"), SymbolToken(Comma)
    )

    val result = parser.parseJStatement().run(tokens)
    result shouldBe Right(List(IdentifierToken("test"), SymbolToken(Comma)), JWhileStatement(
      JExpression(JIntegerTerm(3), List()),
      JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List()))))))
  }

  "parseJDo" should "return new state with JDo" in {
    val tokens = List(
      KeywordToken(Do),
      IdentifierToken("sub"),
      SymbolToken(LeftParen),
      IntToken(4),
      SymbolToken(RightParen),
      SymbolToken(SemiColon)
    )

    val result = parser.parseJStatement().run(tokens)
    result shouldBe Right(List(), JDoStatement(JSubRoutineCall(JBareSubRoutineCall(JName("sub"),
        List(JExpression(JIntegerTerm(4), List()))))))
  }

  it should "handle invalid statements" in {
    val tokens = List(KeywordToken(Return), SymbolToken(Comma), SymbolToken(SemiColon))

    val result = parser.parseJStatement().run(tokens)
    result shouldBe Left("Expected: a valid JTerm token but got: Some(SymbolToken(Comma))")
  }
}
