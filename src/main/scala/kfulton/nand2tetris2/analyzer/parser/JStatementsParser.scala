package kfulton.nand2tetris2.analyzer.parser

import cats.implicits._
import kfulton.nand2tetris2.analyzer.parser.JExpressionsParser._
import kfulton.nand2tetris2.analyzer.parser.JParser.{Parser, _}
import kfulton.nand2tetris2.analyzer.parser.jack._
import kfulton.nand2tetris2.analyzer.tokenizer.tokens._

object JStatementsParser {

    def parseJStatements() =
      for {
        statement <- parseJStatement()
      } yield JStatements(List(statement))

    def parseJStatement(): Parser[JStatement] =
      for {
        t <- peekOption()
        jStatement <- t match {
          case Some(KeywordToken(If)) => parseJIf()
          case Some(KeywordToken(Return)) => parseJReturn()
          case Some(KeywordToken(Do)) => parseJDo()
          case Some(KeywordToken(Let)) => parseJLet()
          case Some(KeywordToken(While)) => parseJWhile()
        }
      } yield jStatement

  private def parseMaybeJLetExpression(): Parser[JExpression] =
    for {
      _ <- matchToken(SymbolToken(LeftSquareBracket))
      jExpression <- parseJExpression()
      _ <- matchToken(SymbolToken(RightSquareBracket))
    } yield jExpression

  private def parseMaybeAlternative(): Parser[JStatements] =
      for {
        _ <- matchToken(KeywordToken(Else))
        _ <- matchToken(SymbolToken(LeftCurlyBracket))
        alternative <- parseJStatements()
        _ <- matchToken(SymbolToken(RightCurlyBracket))
      } yield alternative

  private def parseJLet(): Parser[JLetStatement] =
      for {
        _ <- matchToken(KeywordToken(Let))
        name <- parseJName()
        maybeJExpression <- parseOption[JExpression](List(SymbolToken(LeftSquareBracket)), parseMaybeJLetExpression())
        _ <- matchToken(SymbolToken(Equal))
        jExpression2 <- parseJExpression()
        _ <- matchToken(SymbolToken(SemiColon))
      } yield JLetStatement(name, maybeJExpression, jExpression2)

  private def parseJIf() =
      for {
        _ <- matchToken(KeywordToken(If))
        _ <- matchToken(SymbolToken(LeftParen))
        condition <- parseJExpression()
        _ <- matchToken(SymbolToken(RightParen))
        _ <- matchToken(SymbolToken(LeftCurlyBracket))
        consequence <- parseJStatements()
        _ <- matchToken(SymbolToken(RightCurlyBracket))
        maybeAlternative <- parseOption[JStatements](List(KeywordToken(Else)), parseMaybeAlternative())
      } yield JIfStatement(condition, consequence, maybeAlternative)

  private  def parseJDo(): Parser[JDoStatement] =
        for {
          _ <- matchToken(KeywordToken(Do))
          name <- parseJName()
          jSubRoutineCallType <- parseJSubRoutineCallType(name)
          _ <- matchToken(SymbolToken(SemiColon))
        } yield JDoStatement(JSubRoutineCall(jSubRoutineCallType))

  private def parseJWhile() =
      for {
        _ <- matchToken(KeywordToken(While))
        _ <- matchToken(SymbolToken(LeftParen))
        jExpression <- parseJExpression()
        _ <- matchToken(SymbolToken(RightParen))
        _ <- matchToken(SymbolToken(LeftCurlyBracket))
        jStatements <- parseJStatements()
        _ <- matchToken(SymbolToken(RightCurlyBracket))
      } yield JWhileStatement(jExpression,
        jStatements)

  private def parseJReturn() =
      for {
        _ <- matchToken(KeywordToken(Return))
        expression <- parseJExpression()
        _ <- matchToken(SymbolToken(SemiColon))
      } yield JReturnStatement(expression)
}