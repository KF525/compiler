package kfulton.nand2tetris2.analyzer.parser

import cats.data.StateT
import cats.implicits._
import kfulton.nand2tetris2.analyzer.parser.JExpressionsParser.completedAdditional
import kfulton.nand2tetris2.analyzer.parser.JStatementsParser._
import kfulton.nand2tetris2.analyzer.tokenizer.tokens._

object JParser {
  type Tokens = List[Token]

  type ParseResultOrError[T] = Either[String, T]
  type Parser[T] = StateT[ParseResultOrError, Tokens, T]

  def parseJProgram() = ??? // This is the entry

  def parseJClasses(): Parser[List[JClass]] = ???

  def parseJClass() =
    for {
      _ <- matchToken(KeywordToken(Class))
      jName <- parseJName()
      _ <- matchToken(SymbolToken(LeftCurlyBracket))
      jClassVarDec <- parseJClassVarDecList(List(KeywordToken(Static), KeywordToken(Field)))
      jSubRoutineDec <- parseJSubRoutineDecList(List(KeywordToken(ConstructorKey), KeywordToken(FunctionKey), KeywordToken(MethodKey)))
      _ <- matchToken(SymbolToken(RightCurlyBracket))
    } yield JClass(jName, jClassVarDec, jSubRoutineDec)

  def parseJClassVarDecList(peekTokens: List[Token], list: List[JClassVarDec] = List()): Parser[List[JClassVarDec]] =
    for {
      optionJParameter <- parseOption(peekTokens, parseJClassVarDec)
      jClassVarDecList <- optionJParameter match {
        case Some(add) => parseJClassVarDecList(peekTokens, list :+ add)
        case None => completedAdditional(list)
      }
    } yield jClassVarDecList

  def parseJSubRoutineDecList(peekTokens: List[Token], list: List[JSubRoutineDec] = List()): Parser[List[JSubRoutineDec]] =
    for {
      optionJSubRoutineDec <- parseOption(peekTokens, parseJSubRoutineDec())
      jSubRoutineDecList <- optionJSubRoutineDec match {
        case Some(add) => parseJSubRoutineDecList(peekTokens, list :+ add)
        case None => completedAdditional(list)
      }
    } yield jSubRoutineDecList

  def parseJClassVarDec() =
    for {
     jClassVar <- parseClassVar()
     jType <- parseJType()
     jName <- parseJName()
     additional <- parseAdditionalJNames()
      _ <- matchToken(SymbolToken(SemiColon))
    } yield JClassVarDec(jClassVar, jType, jName, additional)

  def parseClassVar() =
    StateT[ParseResultOrError, Tokens, JClassVar] {
      case KeywordToken(Static) :: remainder => Right((remainder, JStatic))
      case KeywordToken(Field) :: remainder => Right((remainder, JField))
      case t :: remainder => Left(s"Expected: a valid JClassVar Token but got: $t")
      case _ => Left(s"Expected: an JClassVar Token but reached end")
    }

  def parseAdditionalJNames(list: List[JName] = List()): Parser[List[JName]] =
    for {
      optionJParameter <- parseOption(SymbolToken(Comma), parseAdditionalJName())
      jParameterList <- optionJParameter match {
        case Some(add) => parseAdditionalJNames(list :+ add)
        case None => completedAdditional(list)
      }
    } yield jParameterList

  def parseAdditionalJName() =
    for {
      _ <- matchToken(SymbolToken(Comma))
      jName <- parseJName()
    } yield jName

  def parseJType(): Parser[JType] =
    StateT[ParseResultOrError, Tokens, JType] {
      case KeywordToken(BooleanKey) :: remainder => Right((remainder, JType(JBooleanPrimitiveType)))
      case KeywordToken(CharKey) :: remainder => Right((remainder, JType(JCharPrimitiveType)))
      case KeywordToken(IntKey) :: remainder => Right((remainder, JType(JIntPrimitiveType)))
      case IdentifierToken(id) :: remainder => Right((remainder, JType(JClassNameType(id))))
      case t :: remainder => Left(s"Expected: a valid JType Token but got: $t")
      case _ => Left(s"Expected: an JType Token but reached end")
    }

  def parseJSubRoutineDec() =
    for {
      jSubRoutineType <- parsesubRoutineDecType()
      jReturnType <- parseJReturnType()
      name <- parseJName()
      _ <- matchToken(SymbolToken(LeftParen))
      parameterList <- parseJParameterList()
      _ <- matchToken(SymbolToken(RightParen))
      jSubRoutineBody <- parseJSubRoutineBody()
    } yield JSubRoutineDec(jSubRoutineType, jReturnType, name, parameterList, jSubRoutineBody)

  def parsesubRoutineDecType() =
    StateT[ParseResultOrError, Tokens, JSubRoutineType] {
      case KeywordToken(MethodKey) :: remainder => Right((remainder, Method))
      case KeywordToken(FunctionKey) :: remainder => Right((remainder, Function))
      case KeywordToken(ConstructorKey) :: remainder => Right((remainder, Constructor))
      case t :: remainder => Left(s"Expected: a valid JSubRoutineType Token but got: $t")
      case _ => Left(s"Expected: a JSubRoutineType Token but reached end")
    }

  def parseJReturnType(): Parser[JReturnType] =
    StateT[ParseResultOrError, Tokens, JReturnType] {
      case KeywordToken(BooleanKey) :: remainder => Right((remainder, JReturnType(Right(JType(JBooleanPrimitiveType)))))
      case KeywordToken(IntKey) :: remainder => Right((remainder, JReturnType(Right(JType(JIntPrimitiveType)))))
      case KeywordToken(CharKey) :: remainder => Right((remainder, JReturnType(Right(JType(JCharPrimitiveType)))))
      case KeywordToken(VoidKey) :: remainder => Right((remainder, JReturnType(Left(JVoid()))))
    }

  def parseJParameterList() =
    for {
      parameter <- parseJParameter()
      additional <- parseAdditionalJParameters()
    } yield JParameterList(parameter, additional)

  def parseJParameter(): Parser[JParameter] =
    for {
      jType <- parseJType()
      jName <- parseJName()
    } yield JParameter(jType, jName)

  def parseAdditionalJParameter(): Parser[JParameter] =
    for {
      _  <- matchToken(SymbolToken(Comma))
      jType <- parseJType()
      jName <- parseJName()
    } yield JParameter(jType, jName)

  private def parseAdditionalJParameters(list: List[JParameter] = List()): Parser[List[JParameter]] =
    for {
      optionJParameter <- parseOption(SymbolToken(Comma), parseAdditionalJParameter())
      jParameterList <- optionJParameter match {
        case Some(add) => parseAdditionalJParameters(list :+ add)
        case None => completedAdditional(list)
      }
    } yield jParameterList

  def parseJSubRoutineBody() =
    for {
     _ <- matchToken(SymbolToken(LeftCurlyBracket))
     varDecList <- parseAdditionalJVarDecs()
     statements <- parseJStatements()
     _ <- matchToken(SymbolToken(RightCurlyBracket))
    } yield JSubRoutineBody(varDecList, statements)

  private def parseAdditionalJVarDecs(list: List[JVarDec] = List()): Parser[List[JVarDec]] =
    for {
      optionJVarDec <- parseOption(KeywordToken(Var), parseJVarDec())
      jVarDecList <- optionJVarDec match {
        case Some(add) => parseAdditionalJVarDecs(list :+ add)
        case None => completedAdditional(list)
      }
    } yield jVarDecList


  def parseJVarDec() =
    for {
      _ <- matchToken(KeywordToken(Var))
      jType <- parseJType()
      name <- parseJName()
      additional <- parseAdditionalJVars()
    } yield JVarDec(jType, name, additional)

  private def parseAdditionalJVars(list: List[JName] = List()): Parser[List[JName]] =
    for {
      optionJVar <- parseOption(SymbolToken(Comma), parseJVarName())
      jVarList <- optionJVar match {
        case Some(add) => parseAdditionalJVars(list :+ add)
        case None => completedAdditional(list)
      }
    } yield jVarList

  private def parseJVarName(): Parser[JName] =
    for {
      _ <- matchToken(SymbolToken(Comma))
      name  <- parseJName()
    } yield name

  def parseJName(): Parser[JName] =
    StateT[ParseResultOrError, Tokens, JName] {
      case IdentifierToken(id) :: remainder => Right((remainder, JName(id)))
      case t :: remainder => Left(s"Expected: a valid IdentifierToken but got: $t")
      case _ => Left(s"Expected: an IndentifierToken but reached end")
    }

  def matchToken(expected: Token): Parser[Token] =
    StateT[ParseResultOrError, Tokens, Token] {
      case t :: remainder if t == expected => Right((remainder, expected))
      case t :: remainder => Left(s"Expected: $expected but got: $t")
      case _ => Left(s"Expected: $expected but reached end")
    }

  def peekOption(): StateT[ParseResultOrError, Tokens, Option[Token]] =
    StateT[ParseResultOrError, Tokens, Option[Token]]{ s => Right((s, s.headOption)) }

  def peek(): StateT[ParseResultOrError, Tokens, Token] =
    StateT[ParseResultOrError, Tokens, Token]{ s => Right((s, s.head)) }

  def removeToken(): StateT[ParseResultOrError, Tokens, Unit] =
    StateT[ParseResultOrError, Tokens, Unit]{ s => Right((s.tail, ())) }

  def pop(): StateT[ParseResultOrError, Tokens, Token] =
    StateT[ParseResultOrError, Tokens, Token] { s => Right((s.tail, s.head)) }

  def parseOption[T](peekToken: Token, parseF: Parser[T]): StateT[ParseResultOrError, Tokens, Option[T]] =
    for {
      t <- peek()
      option <- t match {
        case y if t == peekToken => parseSome(parseF)
        case n => parseNone()
      }
    } yield option

  def parseOption[T](peekTokens: List[Token], parseF: Parser[T]): StateT[ParseResultOrError, Tokens, Option[T]] =
    for {
      t <- peek()
      option <- t match {
        case y if peekTokens.contains(t) => parseSome(parseF)
        case n => parseNone()
      }
    } yield option

  private def parseSome[T](parseF: Parser[T]): Parser[Option[T]] =
    for {
      t <- parseF
    } yield Option(t)

  private def parseNone[T](): Parser[Option[T]]  =
    StateT[ParseResultOrError, Tokens, Option[T]] { tokens => Right(tokens, None: Option[T]) }
}