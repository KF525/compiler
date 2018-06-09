package kfulton.nand2tetris2.analyzer.parser

import cats.implicits._
import kfulton.nand2tetris2.analyzer.parser.jack._
import kfulton.nand2tetris2.analyzer.tokenizer.tokens._
import org.scalatest.{FlatSpec, Matchers}

class JExpressionsParserTest extends FlatSpec with Matchers {
  val parser = JExpressionsParser

  "parseJTerm" should "return JIntegerTerm" in {
    val result = parser.parseJTerm().run(List(IntToken(3)))
    result shouldBe Right(List(), JIntegerTerm(3))
  }

  it should "return JStringTerm" in {
    val result = parser.parseJTerm().run(List(StringToken("string")))
    result shouldBe Right(List(), JStringTerm("string"))
  }

  it should "return JKeywordTerm" in {
    val tokens = List(KeywordToken(True), KeywordToken(False),
      KeywordToken(Null), KeywordToken(This))
    val result = parser.parseJTerm().run(tokens)
    result shouldBe Right(tokens.tail, JKeywordTerm(JTrue))
    val result2 = parser.parseJTerm().run(tokens.tail)
    result2 shouldBe Right(tokens.tail.tail, JKeywordTerm(JFalse))
    val result3 = parser.parseJTerm().run(tokens.tail.tail)
    result3 shouldBe Right(tokens.tail.tail.tail, JKeywordTerm(JNull))
    val result4 = parser.parseJTerm().run(tokens.tail.tail.tail)
    result4 shouldBe Right(List(), JKeywordTerm(JThis))
  }

  it should "handle invalid keywords" in {
    val tokens = List(KeywordToken(Class))
    val result = parser.parseJTerm().run(tokens)

    result shouldBe Left("Expected: a valid KeywordToken but got: KeywordToken(Class)")
  }

  it should "return new state with JVarNameWithOptionalExpressionTerm" in {
    val tokens = List(IdentifierToken("name"), SymbolToken(LeftSquareBracket), IntToken(3), SymbolToken(RightSquareBracket))

    val result = parser.parseJTerm().run(tokens)
    result shouldBe Right(List(), JVarNameWithOptionalExpressionTerm(JName("name"), Some(JExpression(JIntegerTerm(3), List()))))
  }

  it should "handle JVarNameWithOptionalExpressionTerm with no expression" in {
    val tokens = List(IdentifierToken("name"), SymbolToken(Comma))

    val result = parser.parseJTerm().run(tokens)
    result shouldBe Right(List(SymbolToken(Comma)), JVarNameWithOptionalExpressionTerm(JName("name"), None))
  }

  it should "return new state with JExpressionTerm" in {
    val tokens = List(SymbolToken(LeftParen), IntToken(4), SymbolToken(RightParen))

    val result = parser.parseJTerm().run(tokens)
    result shouldBe Right(List(), JExpressionTerm(JExpression(JIntegerTerm(4), List())))
  }

  it should "return new state with JUnaryOpTerm" in {
    val tokens = List(SymbolToken(Tilda), IntToken(4))
    val tokens2 = List(SymbolToken(Dash), IntToken(5))

    val result = parser.parseJTerm().run(tokens)
    result shouldBe Right(List(), JUnaryOpTerm(JTilda, JIntegerTerm(4)))
    val result2 = parser.parseJTerm().run(tokens2)
    result2 shouldBe Right(List(), JUnaryOpTerm(JDash, JIntegerTerm(5)))
  }

  "parseOp" should "return new state with JOp" in {
    val tokens = List(SymbolToken(Plus), SymbolToken(Minus), SymbolToken(Asterisk),
      SymbolToken(Slash), SymbolToken(Amp), SymbolToken(Pipe), SymbolToken(LessThan),
      SymbolToken(GreaterThan), SymbolToken(Equal)
    )

    val result = parser.parseOp().run(tokens)
    result shouldBe Right(tokens.tail, JPlus)
    val result2 = parser.parseOp().run(tokens.tail)
    result2 shouldBe Right(tokens.tail.tail, JMinus)
    val result3 = parser.parseOp().run(tokens.tail.tail)
    result3 shouldBe Right(tokens.tail.tail.tail, JAsterisk)
    val result4 = parser.parseOp().run(tokens.tail.tail.tail)
    result4 shouldBe Right(tokens.tail.tail.tail.tail, JSlash)
    val result5 = parser.parseOp().run(tokens.tail.tail.tail.tail)
    result5 shouldBe Right(tokens.tail.tail.tail.tail.tail, JAmp)
    val result6 = parser.parseOp().run(tokens.tail.tail.tail.tail.tail)
    result6 shouldBe Right(tokens.tail.tail.tail.tail.tail.tail, JPipe)
    val result7 = parser.parseOp().run(tokens.tail.tail.tail.tail.tail.tail)
    result7 shouldBe Right(tokens.tail.tail.tail.tail.tail.tail.tail, JLessThan)
    val result8 = parser.parseOp().run(tokens.tail.tail.tail.tail.tail.tail.tail)
    result8 shouldBe Right(tokens.tail.tail.tail.tail.tail.tail.tail.tail, JGreaterThan)
    val result9 = parser.parseOp().run(tokens.tail.tail.tail.tail.tail.tail.tail.tail)
    result9 shouldBe Right(List(), JEqual)
  }

  it should "handle invalid tokens" in {
    val tokens = List(SymbolToken(Tilda))

    val result = parser.parseOp().run(tokens)
    result shouldBe Left("Expected: a valid SymbolToken but got: SymbolToken(Tilda)")
  }

  it should "return new state with SubroutineExpressionTerm" in {
    val tokens = List(IdentifierToken("name"),
      SymbolToken(LeftParen),
      IntToken(4),
      SymbolToken(RightParen))

    val result = parser.parseJTerm().run(tokens)
    result shouldBe Right(List(),
      JSubRoutineExpressionTerm(JBareSubRoutineCall(
        JName("name"),
        List(JExpression(JIntegerTerm(4), List())))))
  }

  "parseSubRoutineCall" should "return new state with Bare Call" in {
    val tokens = List(
      SymbolToken(LeftParen),
      IntToken(4),
      SymbolToken(RightParen))

    val result = parser.parseJSubRoutineCallType(JName("name")).run(tokens)
    result shouldBe Right(List(),
      JBareSubRoutineCall(
        JName("name"),
        List(JExpression(JIntegerTerm(4), List()))))
  }

  it should "return new state with Class Call" in {
    val tokens = List(
      SymbolToken(Period),
      IdentifierToken("subname"),
      SymbolToken(LeftParen),
      IntToken(4),
      SymbolToken(RightParen))

    val result = parser.parseJSubRoutineCallType(JName("name")).run(tokens)
    result shouldBe Right(List(), JClassSubroutineCall(JName("name"), JName("subname"),
      List(JExpression(JIntegerTerm(4), List()))))
  }

  "parseJExpression" should "return new state with JExpression" in {
    val tokens = List(IntToken(4), IdentifierToken("id"))

    val result = parser.parseJExpression().run(tokens)
    result shouldBe Right(List(IdentifierToken("id")), JExpression(JIntegerTerm(4), List()))
  }

  it should "handle additional JOps and JTerms" in {
    val tokens = List(IntToken(4), SymbolToken(Plus), IntToken(5), IdentifierToken("id"))

    val result = parser.parseJExpression().run(tokens)
    result shouldBe Right((List(IdentifierToken("id")),JExpression(JIntegerTerm(4),List(JOpTerm(JPlus,JIntegerTerm(5))))))
  }

  "parseJExpressionList" should "return new state with JExpressionList" in {
    val tokens = List(IntToken(5), SymbolToken(Plus), IntToken(5), KeywordToken(Var))

    val result = parser.parseJExpressionList().run(tokens)
    result shouldBe Right((List(KeywordToken(Var)),List(JExpression(JIntegerTerm(5),List(JOpTerm(JPlus,JIntegerTerm(5)))))))
  }

  it should "handle more than one JExpressions" in {
    val tokens = List(IntToken(6), SymbolToken(Minus), IntToken(3),
      SymbolToken(Comma), IntToken(5), SymbolToken(Plus), IntToken(5), KeywordToken(Var))

    val result = parser.parseJExpressionList().run(tokens)
    result shouldBe Right((List(IntToken(5), SymbolToken(Plus), IntToken(5), KeywordToken(Var)),List(JExpression(JIntegerTerm(6),List(JOpTerm(JMinus,JIntegerTerm(3)))))))
  }
}