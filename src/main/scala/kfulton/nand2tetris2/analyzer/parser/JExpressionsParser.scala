package kfulton.nand2tetris2.analyzer.parser

import cats.data.StateT
import cats.implicits._
import kfulton.nand2tetris2.analyzer.parser.JParser.{ParseResultOrError, Parser, Tokens, _}
import kfulton.nand2tetris2.analyzer.parser.jack._
import kfulton.nand2tetris2.analyzer.tokenizer.tokens._

object JExpressionsParser {

  def parseJExpression(): Parser[JExpression] =
    for {
      jTerm <- parseJTerm()
      additional <- parseAdditional[JOpTerm](List(SymbolToken(Plus), SymbolToken(Minus), SymbolToken(Asterisk),
        SymbolToken(Slash), SymbolToken(Amp), SymbolToken(Pipe), SymbolToken(GreaterThan), SymbolToken(LessThan),
        SymbolToken(Equal)), parseOpTerm())
    } yield jack.JExpression(jTerm, additional)


  def parseJExpressionList(): Parser[List[JExpression]] =
    for {
      jExpression <- parseDelimitedList[JExpression](List(SymbolToken(Tilda), SymbolToken(Dash), SymbolToken(LeftParen)), parseJExpression(),
        peekIncludesIdentifiers = true, peekIncludesInts = true, peekIncludesKeywords = true, peekIncludesStrings = true)
    } yield jExpression

  private def parseOpTerm() =
    for {
      op <- parseOp()
      jTerm <- parseJTerm()
    } yield JOpTerm(op, jTerm)

  def parseJTerm(): Parser[JTerm] =
    for {
      t <- peekOption()
      jTerm <- t match {
        case Some(IntToken(i)) => parseJIntegerTerm()
        case Some(StringToken(s)) => parseJStringTerm()
        case Some(KeywordToken(k)) => parseJKeywordTerm()
        case Some(SymbolToken(LeftParen)) => parseJExpressionTerm()
        case Some(SymbolToken(Dash)) => parseJUnaryOpTerm()
        case Some(SymbolToken(Tilda)) => parseJUnaryOpTerm()
        case Some(IdentifierToken(id)) => parseIdentifierTerms()
        case _ =>  StateT[ParseResultOrError, Tokens, JTerm] { tokens => Left(s"Expected: a valid JTerm token but got: $t") }
      }
    } yield jTerm

  private def parseJExpressionTerm(): Parser[JExpressionTerm] =
    for {
      _ <- matchToken(SymbolToken(LeftParen))
      jExpression <- parseJExpression()
      _ <- matchToken(SymbolToken(RightParen))
    } yield JExpressionTerm(jExpression)

  private def parseIdentifierTerms(): Parser[JTerm] =
    for {
      name <- parseJName()
      t <- peekOption()
      jTerm <- t match {
        case Some(subBare) if subBare.equals(SymbolToken(LeftParen)) => parseJSubRoutineExpressionTerm(name)
        case Some(subClass) if subClass.equals(SymbolToken(Period)) => parseJSubRoutineExpressionTerm(name)
        case _ => parseJVarNameWithOptionalExpressionTerm(name)
      }
    } yield jTerm

  private def parseJVarNameWithOptionalExpressionTerm(name: JName): Parser[JVarNameWithOptionalExpressionTerm] =
    for {
      t <- peekOption()
      optionJExpression <- parseOption[JExpression](List(SymbolToken(LeftSquareBracket)), parseOptionalJExpression())
    } yield JVarNameWithOptionalExpressionTerm(name, optionJExpression)

  private def parseOptionalJExpression() =
    for {
      _ <- matchToken(SymbolToken(LeftSquareBracket))
      jExpression <- parseJExpression()
      _ <- matchToken(SymbolToken(RightSquareBracket))
    } yield jExpression

  private def parseJUnaryOpTerm(): Parser[JUnaryOpTerm] = {
    for {
      unary <- parseJUnaryOp()
      jterm <- parseJTerm()
    } yield JUnaryOpTerm(unary, jterm)
  }

  private def parseJSubRoutineExpressionTerm(name: JName): Parser[JSubRoutineExpressionTerm] =
    for {
      subroutine <- parseJSubRoutineCallType(name)
    } yield JSubRoutineExpressionTerm(subroutine)

  def parseJSubRoutineCallType(name: JName): Parser[JSubRoutineCallType] =
    for {
      t <- peekOption()
      subroutine <- t match {
        case Some(subBare) if subBare.equals(SymbolToken(LeftParen)) => parseJBareSubRoutineCall(name)
        case _ => parseJClassSubRoutineCall(name)
      }
    } yield subroutine

  private def parseJBareSubRoutineCall(name: JName): Parser[JBareSubRoutineCall] =
      for {
        _ <- matchToken(SymbolToken(LeftParen))
        jExpressionList <- parseJExpressionList()
        _ <- matchToken(SymbolToken(RightParen))
      } yield JBareSubRoutineCall(name, jExpressionList)

  private def parseJClassSubRoutineCall(name: JName) =
    for {
      _ <- matchToken(SymbolToken(Period))
      name2 <- parseJName()
      _ <- matchToken(SymbolToken(LeftParen))
      jExpression <- parseJExpressionList()
      _ <- matchToken(SymbolToken(RightParen))
    } yield JClassSubroutineCall(name, name2, jExpression)

  private def parseJIntegerTerm(): Parser[JIntegerTerm] =
    StateT[ParseResultOrError, Tokens, JIntegerTerm] {
      case IntToken(i) :: remainder => Right((remainder, JIntegerTerm(i)))
      case t :: remainder => Left(s"Expected an IntToken but got: $t")
      case _ => Left(s"Expected: an IntToken but reached end")
    }

  private def parseJStringTerm(): Parser[JStringTerm] =
    StateT[ParseResultOrError, Tokens, JStringTerm] {
      case StringToken(s) :: remainder => Right((remainder, JStringTerm(s)))
      case t :: remainder => Left(s"Expected an StringToken but got: $t")
      case _ => Left(s"Expected: an StringToken but reached end")
    }

  private def parseJKeywordTerm(): Parser[JKeywordTerm] =
    StateT[ParseResultOrError, Tokens, JKeywordTerm] {
      case KeywordToken(True) :: remainder => Right((remainder, JKeywordTerm(JTrue)))
      case KeywordToken(False) :: remainder => Right((remainder, JKeywordTerm(JFalse)))
      case KeywordToken(Null) :: remainder => Right((remainder, JKeywordTerm(JNull)))
      case KeywordToken(This) :: remainder => Right((remainder, JKeywordTerm(JThis)))
      case t :: remainder => Left(s"Expected: a valid KeywordToken but got: $t")
      case _ => Left(s"Expected: an KeywordToken but reached end")
    }

  private def parseJUnaryOp() =
    StateT[ParseResultOrError, Tokens, JUnaryOp] {
      case SymbolToken(Dash) :: remainder => Right((remainder, JDash))
      case SymbolToken(Tilda) :: remainder => Right((remainder, JTilda))
      case t :: remainder => Left(s"Expected: a valid SymbolToken but got: $t")
      case _ => Left(s"Expected: an StringToken but reached end")
    }

  def parseOp(): Parser[JOp] =
    StateT[ParseResultOrError, Tokens, JOp] {
      case SymbolToken(Plus) :: remainder => Right((remainder, JPlus))
      case SymbolToken(Minus) :: remainder => Right((remainder, JMinus))
      case SymbolToken(Asterisk) :: remainder => Right((remainder, JAsterisk))
      case SymbolToken(Slash) :: remainder => Right((remainder, JSlash))
      case SymbolToken(Amp) :: remainder => Right((remainder, JAmp))
      case SymbolToken(Pipe) :: remainder => Right((remainder, JPipe))
      case SymbolToken(LessThan) :: remainder => Right((remainder, JLessThan))
      case SymbolToken(GreaterThan) :: remainder => Right((remainder, JGreaterThan))
      case SymbolToken(Equal) :: remainder => Right((remainder, JEqual))
      case t::remainder => Left(s"Expected: a valid SymbolToken but got: $t")
      case _ => Left(s"Expected: a SymbolToken but reached end")
    }
}