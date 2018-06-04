package kfulton.nand2tetris2.analyzer.parser

import cats.implicits._
import kfulton.nand2tetris2.analyzer.tokenizer.tokens._
import org.scalatest.{FlatSpec, Matchers}

class JParserTest extends FlatSpec with Matchers {
  val parser = JParser

  "parseJClasses" should "return new state with List[JClass]" in {
//    val tokens = List(KeywordToken(Class), IdentifierToken("className"), SymbolToken(LeftCurlyBracket),
//      KeywordToken(Static), KeywordToken(BooleanKey), IdentifierToken("name"), SymbolToken(SemiColon),
//      KeywordToken(Static), KeywordToken(BooleanKey), IdentifierToken("name"), SymbolToken(SemiColon),
//      KeywordToken(MethodKey), KeywordToken(BooleanKey), IdentifierToken("subName"),
//      SymbolToken(LeftParen), KeywordToken(BooleanKey), IdentifierToken("parameter1"),
//      SymbolToken(RightParen), SymbolToken(LeftCurlyBracket), KeywordToken(Return),
//      IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket),
//      KeywordToken(MethodKey), KeywordToken(BooleanKey), IdentifierToken("subName"),
//      SymbolToken(LeftParen), KeywordToken(BooleanKey), IdentifierToken("parameter1"),
//      SymbolToken(RightParen), SymbolToken(LeftCurlyBracket), KeywordToken(Return),
//      IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket), SymbolToken(RightCurlyBracket),
//      KeywordToken(Class), IdentifierToken("className"), SymbolToken(LeftCurlyBracket),
//      KeywordToken(Static), KeywordToken(BooleanKey), IdentifierToken("name"), SymbolToken(SemiColon),
//      KeywordToken(Static), KeywordToken(BooleanKey), IdentifierToken("name"), SymbolToken(SemiColon),
//      KeywordToken(MethodKey), KeywordToken(BooleanKey), IdentifierToken("subName"),
//      SymbolToken(LeftParen), KeywordToken(BooleanKey), IdentifierToken("parameter1"),
//      SymbolToken(RightParen), SymbolToken(LeftCurlyBracket), KeywordToken(Return),
//      IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket),
//      KeywordToken(MethodKey), KeywordToken(BooleanKey), IdentifierToken("subName"),
//      SymbolToken(LeftParen), KeywordToken(BooleanKey), IdentifierToken("parameter1"),
//      SymbolToken(RightParen), SymbolToken(LeftCurlyBracket), KeywordToken(Return),
//      IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket), SymbolToken(RightCurlyBracket)
//    )
//
//    val result = parser.parseJClasses().run(tokens)
//    result shouldBe Right(List(), List(JClass(JName("className"),
//      List(JClassVarDec(JStatic, JType(JBooleanPrimitiveType), JName("name"), List()),
//        JClassVarDec(JStatic, JType(JBooleanPrimitiveType), JName("name"), List())),
//      List( JSubRoutineDec(JMethod, JReturnType(Right(JType(JBooleanPrimitiveType))), JName("subName"),
//        JParameterList(JParameter(JType(JBooleanPrimitiveType), JName("parameter1")), List()),
//        JSubRoutineBody(List(), JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List())))))),
//        JSubRoutineDec(JMethod, JReturnType(Right(JType(JBooleanPrimitiveType))), JName("subName"),
//          JParameterList(JParameter(JType(JBooleanPrimitiveType), JName("parameter1")), List()),
//          JSubRoutineBody(List(),JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List())))))))),
//      JClass(JName("className"),
//      List(JClassVarDec(JStatic, JType(JBooleanPrimitiveType), JName("name"), List()),
//        JClassVarDec(JStatic, JType(JBooleanPrimitiveType), JName("name"), List())),
//      List( JSubRoutineDec(JMethod, JReturnType(Right(JType(JBooleanPrimitiveType))), JName("subName"),
//        JParameterList(JParameter(JType(JBooleanPrimitiveType), JName("parameter1")), List()),
//        JSubRoutineBody(List(), JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List())))))),
//        JSubRoutineDec(JMethod, JReturnType(Right(JType(JBooleanPrimitiveType))), JName("subName"),
//          JParameterList(JParameter(JType(JBooleanPrimitiveType), JName("parameter1")), List()),
//          JSubRoutineBody(List(),JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List()))))))))))
  }

  "parseJClass" should "return new state with JClass" in {
    val tokens = List(KeywordToken(Class), IdentifierToken("className"), SymbolToken(LeftCurlyBracket),
      KeywordToken(Static), KeywordToken(BooleanKey), IdentifierToken("name"), SymbolToken(SemiColon),
      KeywordToken(Static), KeywordToken(BooleanKey), IdentifierToken("name"), SymbolToken(SemiColon),
      KeywordToken(MethodKey), KeywordToken(BooleanKey), IdentifierToken("subName"),
      SymbolToken(LeftParen), KeywordToken(BooleanKey), IdentifierToken("parameter1"),
      SymbolToken(RightParen), SymbolToken(LeftCurlyBracket), KeywordToken(Return),
      IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket),
      KeywordToken(MethodKey), KeywordToken(BooleanKey), IdentifierToken("subName"),
      SymbolToken(LeftParen), KeywordToken(BooleanKey), IdentifierToken("parameter1"),
      SymbolToken(RightParen), SymbolToken(LeftCurlyBracket), KeywordToken(Return),
      IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket), SymbolToken(RightCurlyBracket))

    val result = parser.parseJClass().run(tokens)
    result shouldBe Right(List(), JClass(JName("className"),
      List(JClassVarDec(JStatic, JType(JBooleanPrimitiveType), JName("name"), List()),
        JClassVarDec(JStatic, JType(JBooleanPrimitiveType), JName("name"), List())),
      List( JSubRoutineDec(JMethod, JReturnType(Right(JType(JBooleanPrimitiveType))), JName("subName"),
        JParameterList(JParameter(JType(JBooleanPrimitiveType), JName("parameter1")), List()),
        JSubRoutineBody(List(), JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List())))))),
        JSubRoutineDec(JMethod, JReturnType(Right(JType(JBooleanPrimitiveType))), JName("subName"),
          JParameterList(JParameter(JType(JBooleanPrimitiveType), JName("parameter1")), List()),
          JSubRoutineBody(List(),JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List())))))))))
  }

  it should "handle no JClassVarDec or JSubRoutineDec" in {
    val tokens = List(KeywordToken(Class), IdentifierToken("className"), SymbolToken(LeftCurlyBracket),
      SymbolToken(RightCurlyBracket))

    val result = parser.parseJClass().run(tokens)
    result shouldBe Right(List(), JClass(JName("className"), List(), List()))
  }

  "parseJClassVarDec" should "return new state with JClassVarDec" in {
    val tokens = List(KeywordToken(Static), KeywordToken(BooleanKey), IdentifierToken("name"), SymbolToken(SemiColon), SymbolToken(Equal))

    val result = parser.parseJClassVarDec().run(tokens)
    result shouldBe Right(List(SymbolToken(Equal)), JClassVarDec(JStatic, JType(JBooleanPrimitiveType), JName("name"), List()))
  }

  it should "handle multiple JNames" in {
    val tokens = List(KeywordToken(Static), KeywordToken(BooleanKey), IdentifierToken("name"),
      SymbolToken(Comma), IdentifierToken("name2"), SymbolToken(Comma), IdentifierToken("name3"), SymbolToken(SemiColon), SymbolToken(Equal))

    val result = parser.parseJClassVarDec().run(tokens)
    result shouldBe Right(List(SymbolToken(Equal)), JClassVarDec(JStatic, JType(JBooleanPrimitiveType), JName("name"), List(JName("name2"), JName("name3"))))
  }

  "parseJType" should "return new state with JType" in {
    val tokens = List(KeywordToken(IntKey),
      KeywordToken(BooleanKey), KeywordToken(CharKey), IdentifierToken("id"))

    val result = parser.parseJType().run(tokens)
    result shouldBe Right(List(KeywordToken(BooleanKey), KeywordToken(CharKey), IdentifierToken("id")), JType(JIntPrimitiveType))
    val result2 = parser.parseJType().run(tokens.tail)
    result2 shouldBe Right(List(KeywordToken(CharKey), IdentifierToken("id")), JType(JBooleanPrimitiveType))
    val result3 = parser.parseJType().run(tokens.tail.tail)
    result3 shouldBe Right(List(IdentifierToken("id")), JType(JCharPrimitiveType))
    val result4 = parser.parseJType().run(tokens.tail.tail.tail)
    result4 shouldBe Right(List(), JType(JClassNameType("id")))
  }

  it should "handle errors" in {
    val tokens = List(KeywordToken(Var))

    val result = parser.parseJType().run(tokens)
    result shouldBe Left("Expected: a valid JType Token but got: KeywordToken(Var)")
  }

  "parseSubRoutineDec" should "return new state with JSubRoutineDec" in {
    val tokens = List(KeywordToken(MethodKey), KeywordToken(BooleanKey), IdentifierToken("subName"),
    SymbolToken(LeftParen), KeywordToken(BooleanKey), IdentifierToken("parameter1"),
    SymbolToken(RightParen), SymbolToken(LeftCurlyBracket), KeywordToken(Return),
      IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket), SymbolToken(Equal))

    val result = parser.parseJSubRoutineDec().run(tokens)
    result shouldBe Right(List(SymbolToken(Equal)),
      JSubRoutineDec(JMethod,
      JReturnType(Right(JType(JBooleanPrimitiveType))),
      JName("subName"),
      JParameterList(JParameter(JType(JBooleanPrimitiveType), JName("parameter1")), List()),
      JSubRoutineBody(List(),
      JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List())))))))
  }

  "parseJReturnType" should "return new state with JReturnType" in {
    val tokens = List(KeywordToken(BooleanKey), KeywordToken(IntKey), KeywordToken(CharKey), KeywordToken(VoidKey))

    val result = parser.parseJReturnType().run(tokens)
    result shouldBe Right(tokens.tail, JReturnType(Right(JType(JBooleanPrimitiveType))))
    val result2 = parser.parseJReturnType().run(tokens.tail)
    result2 shouldBe Right(tokens.tail.tail, JReturnType(Right(JType(JIntPrimitiveType))))
    val result3 = parser.parseJReturnType().run(tokens.tail.tail)
    result3 shouldBe Right(tokens.tail.tail.tail, JReturnType(Right(JType(JCharPrimitiveType))))
    val result4 = parser.parseJReturnType().run(tokens.tail.tail.tail)
    result4 shouldBe Right(List(), JReturnType(Left(JVoid())))
  }

  "parseJParameterList" should "return new state with JParameterList" in {
    val tokens = List(KeywordToken(BooleanKey), IdentifierToken("parameter1"), SymbolToken(Equal))

    val result = parser.parseJParameterList().run(tokens)
    result shouldBe Right(List(SymbolToken(Equal)),
      JParameterList(JParameter(JType(JBooleanPrimitiveType), JName("parameter1")), List()))
  }

  it should "handle multiple parameters" in {
    val tokens = List(KeywordToken(BooleanKey), IdentifierToken("parameter1"),
      SymbolToken(Comma), KeywordToken(IntKey), IdentifierToken("parameter2"),
      SymbolToken(Comma), IdentifierToken("class"), IdentifierToken("parameter3"),
      SymbolToken(Equal))

    val result = parser.parseJParameterList().run(tokens)
    result shouldBe Right(List(SymbolToken(Equal)),
      JParameterList(JParameter(JType(JBooleanPrimitiveType), JName("parameter1")),
        List(
          JParameter(JType(JIntPrimitiveType), JName("parameter2")),
          JParameter(JType(JClassNameType("class")), JName("parameter3"))
        )))
  }

  "parseSubRoutineBody" should "return new state with JSubRoutineBody" in {
    val tokens = List(SymbolToken(LeftCurlyBracket), KeywordToken(Return),
      IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket) )

    val result = parser.parseJSubRoutineBody().run(tokens)
    result shouldBe Right(List(), JSubRoutineBody(List(), JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List()))))))
  }

  it should "handle the presence of JVarDecs" in {
    val tokens = List(SymbolToken(LeftCurlyBracket), KeywordToken(Var), KeywordToken(BooleanKey), IdentifierToken("name"),
      KeywordToken(Return), IntToken(4), SymbolToken(SemiColon), SymbolToken(RightCurlyBracket))

    val result = parser.parseJSubRoutineBody().run(tokens)
    result shouldBe Right(List(), JSubRoutineBody(List(JVarDec(JType(JBooleanPrimitiveType), JName("name"), List())),
      JStatements(List(JReturnStatement(JExpression(JIntegerTerm(4), List()))))))
  }

  "parseJVarDec" should "return new state with JVarDec" in {
    val tokens = List(KeywordToken(Var), KeywordToken(IntKey), IdentifierToken("name"), SymbolToken(Period))

    val result = parser.parseJVarDec().run(tokens)
    result shouldBe Right(List(SymbolToken(Period)), JVarDec(JType(JIntPrimitiveType), JName("name"), List()))
  }

  it should "handle multiple jVarNames" in {
    val tokens = List(KeywordToken(Var), KeywordToken(IntKey), IdentifierToken("name"),
      SymbolToken(Comma), IdentifierToken("name2"), SymbolToken(Comma), IdentifierToken("name3"), SymbolToken(Period))

    val result = parser.parseJVarDec().run(tokens)
    result shouldBe Right(List(SymbolToken(Period)), JVarDec(JType(JIntPrimitiveType), JName("name"), List(JName("name2"), JName("name3"))))
  }

  "matchTokens" should "return new state if successful" in {
    val result = parser.matchToken(IntToken(3)).run(List(IntToken(3)))
    result shouldBe Right(List(), IntToken(3))
  }

  it should "handle failures" in {
    val result = parser.matchToken(SymbolToken(Comma)).run(List(SymbolToken(Period)))
    result shouldBe Left("Expected: SymbolToken(Comma) but got: SymbolToken(Period)")
  }

  it should "handle empty tokens" in {
    val result = parser.matchToken(SymbolToken(Comma)).run(List())
    result shouldBe Left("Expected: SymbolToken(Comma) but reached end")
  }

  "parseJName" should "return new state with JName" in {
    val tokens = List(IdentifierToken("name"))

    val result = parser.parseJName().run(tokens)
    result shouldBe Right(List(), JName("name"))
  }

  it should "handle failures" in {
    val tokens = List(StringToken("string"))

    val result = parser.parseJName().run(tokens)
    result shouldBe Left("Expected: a valid IdentifierToken but got: StringToken(string)")
  }

  "peekOption" should "look at the next token" in {
    val tokens = List(StringToken("string"))

    val result = parser.peekOption().run(tokens)
    result shouldBe Right(List(StringToken("string")), Some(StringToken("string")))
  }

  it should "handle an empty list" in {
    val tokens = List()

    val result = parser.peekOption().run(tokens)
    result shouldBe Right(List(), None)
  }

  "pop" should "remove and return the next token" in {
    pending
  }

  "removeToken" should "remove the next token" in {
   pending
  }
}