package kfulton.nand2tetris2.analyzer.parser

import cats.data.StateT
import cats.implicits._
import kfulton.nand2tetris2.analyzer.parser.JStatementsParser._
import kfulton.nand2tetris2.analyzer.tokenizer.tokens._

object JParser {
  type Tokens = List[Token]
  type ParseResultOrError[T] = Either[String, T]
  type Parser[T] = StateT[ParseResultOrError, Tokens, T]

  def parseJProgram(tokens: List[Token]) = parseJClasses().run(tokens)

  def parseJClasses(): Parser[List[JClass]] =
    for {
      jClass <- parseJClass()
      additionalJClasses <- parseAdditional[JClass](List(KeywordToken(Class)), parseJClass())
   } yield jClass +: additionalJClasses

  def parseJClass() =
    for {
      _ <- matchToken(KeywordToken(Class))
      jName <- parseJName()
      _ <- matchToken(SymbolToken(LeftCurlyBracket))
      jClassVarDec <- parseJClassVarDecList(List(KeywordToken(Static), KeywordToken(Field))) //TODO: IdentifierToken
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
     additional <- parseAdditional[JName](List(SymbolToken(Comma)), parseAdditionalJName())
      _ <- matchToken(SymbolToken(SemiColon))
    } yield JClassVarDec(jClassVar, jType, jName, additional)

  def parseClassVar() =
    StateT[ParseResultOrError, Tokens, JClassVar] {
      case KeywordToken(Static) :: remainder => Right((remainder, JStatic))
      case KeywordToken(Field) :: remainder => Right((remainder, JField))
      case t :: remainder => Left(s"Expected: a valid JClassVar Token but got: $t")
      case _ => Left(s"Expected: an JClassVar Token but reached end")
    }

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
      parameterList <- parseJParameterList(List(KeywordToken(CharKey), KeywordToken(BooleanKey), KeywordToken(IntKey)), parseJParameter())
      _ <- matchToken(SymbolToken(RightParen))
      jSubRoutineBody <- parseJSubRoutineBody()
    } yield JSubRoutineDec(jSubRoutineType, jReturnType, name, parameterList, jSubRoutineBody)

  def parsesubRoutineDecType() =
    StateT[ParseResultOrError, Tokens, JSubRoutineType] {
      case KeywordToken(MethodKey) :: remainder => Right((remainder, JMethod))
      case KeywordToken(FunctionKey) :: remainder => Right((remainder, JFunction))
      case KeywordToken(ConstructorKey) :: remainder => Right((remainder, JConstructor))
      case t :: remainder => Left(s"Expected: a valid JSubRoutineType Token but got: $t")
      case _ => Left(s"Expected: a JSubRoutineType Token but reached end")
    }

  def parseJReturnType(): Parser[JReturnType] =
    StateT[ParseResultOrError, Tokens, JReturnType] {
      case KeywordToken(BooleanKey) :: remainder => Right((remainder, JReturnType(Right(JType(JBooleanPrimitiveType)))))
      case KeywordToken(IntKey) :: remainder => Right((remainder, JReturnType(Right(JType(JIntPrimitiveType)))))
      case KeywordToken(CharKey) :: remainder => Right((remainder, JReturnType(Right(JType(JCharPrimitiveType)))))
      case KeywordToken(VoidKey) :: remainder => Right((remainder, JReturnType(Left(JVoid()))))
      case t :: remainder => Left(s"Expected: a valid JReturnType Token but got: $t")
      case _ => Left(s"Expected: a JReturnType Token but reached end")
    }

  def parseJParameterList(peekTokens: List[Token], parseF: Parser[JParameter]): Parser[List[JParameter]] =
    for {
     jParameterList <- parseDelimitedList[JParameter](peekTokens, parseF)
    } yield jParameterList

  def parseJParameter(): Parser[JParameter] =
    for {
      jType <- parseJType()
      jName <- parseJName()
    } yield JParameter(jType, jName)

  def parseJSubRoutineBody() =
    for {
     _ <- matchToken(SymbolToken(LeftCurlyBracket))
     varDecList <- parseAdditional[JVarDec](List(KeywordToken(Var)), parseJVarDec())
     statements <- parseJStatements()
     _ <- matchToken(SymbolToken(RightCurlyBracket))
    } yield JSubRoutineBody(varDecList, statements)

  def parseJVarDec() =
    for {
      _ <- matchToken(KeywordToken(Var))
      jType <- parseJType()
      name <- parseJName()
      additional <- parseAdditional[JName](List(SymbolToken(Comma)), parseJVarName())
    } yield JVarDec(jType, name, additional)

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

  def parseDelimitedList[T](peekTokens: List[Token], parseF: Parser[T], delimiter: Token = SymbolToken(Comma),
                            list: List[T] = List()): Parser[List[T]] =
    for {
      optionT <- parseOption(peekTokens, parseF, true)
      tList <- optionT match {
        case Some(t) => parseDelimitedRemainingList(peekTokens, delimiter, parseF, t +:list)
        case None => completedAdditional(list)
      }
    } yield tList

  def parseDelimitedRemainingList[T](peekTokens: List[Token], delimiter: Token = SymbolToken(Comma),
                                     parseF: Parser[T], list: List[T]): Parser[List[T]] =
    for {
      t <- peekOption()
      jList <- t match {
        case Some(t) if t.equals(delimiter) => getNextInList(peekTokens, delimiter, parseF, list)
        case Some(t) => completedAdditional(list)
        case None => completedAdditional(list)
      }
    } yield jList

   def getNextInList[T](peekTokens: List[Token], delimiter: Token = SymbolToken(Comma),
      parseF: Parser[T], list: List[T]): Parser[List[T]] =
     for {
      _ <- matchToken(delimiter)
      optionT <- parseOption(peekTokens, parseF, true)
      tList <- optionT match {
        case Some(t) => parseDelimitedRemainingList(peekTokens, delimiter, parseF, list :+ t)
        case None => completedAdditional(list)
      }
    } yield tList

  def parseAdditional[T](peekTokens: List[Token], parseF: Parser[T], list: List[T] = List()): Parser[List[T]] =
    for {
      optionT <- parseOption(peekTokens, parseF)
      tList <- optionT match {
        case Some(t) => parseAdditional[T](peekTokens, parseF, list :+ t)
        case None => completedAdditional(list)
      }
    } yield tList

  def completedAdditional[T](list: List[T]): StateT[ParseResultOrError, Tokens, List[T]] =
    StateT[ParseResultOrError, Tokens, List[T]]{tokens => Right(tokens, list) }

  def peekOption(): StateT[ParseResultOrError, Tokens, Option[Token]] =
    StateT[ParseResultOrError, Tokens, Option[Token]]{ tokens => Right((tokens, tokens.headOption)) }

  def removeToken(): StateT[ParseResultOrError, Tokens, Unit] =
    StateT[ParseResultOrError, Tokens, Unit]{ tokens => Right((tokens.tail, ())) }

  def pop(): StateT[ParseResultOrError, Tokens, Token] =
    StateT[ParseResultOrError, Tokens, Token] { tokens => Right((tokens.tail, tokens.head)) }

  def parseOption[T](peekTokens: List[Token], parseF: Parser[T], peekIncludesIdentifiers: Boolean = false): StateT[ParseResultOrError, Tokens, Option[T]] =
    for {
      t <- peekOption()
      option <- t match {
        case y if t.isDefined && (peekTokens.contains(t.get) || (peekIncludesIdentifiers && t.get.isInstanceOf[IdentifierToken])) => parseSome(parseF)
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