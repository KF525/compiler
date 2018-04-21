package kfulton.nand2tetris2.analyzer.tokenizer

import kfulton.nand2tetris2.analyzer.tokenizer.tokens._

class Tokenizer {

  def advance(lines: Stream[String], tokens: Stream[Either[Token, TokenizerError]] = Stream.empty,
              insideMultiLineComment: Boolean = false): Stream[Either[Token, TokenizerError]] = {
    val (finalTokens, _) = lines.foldLeft((tokens, insideMultiLineComment)) {
      (currentTokens, line) =>
        val chars = line.toCharArray.toList
        val (newTokens, insideMLComment) = tokenizeLine(chars.head.toString, chars.tail, currentTokens._2, currentTokens._1)
        (Stream.concat(tokens, newTokens), insideMLComment)
    }
    finalTokens
  }

  //TODO: Losing some tokens
  def tokenizeLine(current: String, lines: List[Char], insideMLComment: Boolean,
                   tokens: Stream[Either[Token, TokenizerError]] = Stream.empty): (Stream[Either[Token, TokenizerError]], Boolean) =
    (lines, insideMLComment) match {
      case (Nil, true) => (tokens, insideMLComment)
      case (Nil, false) if tokenType(current).isLeft => (Stream.concat(tokens, Stream.apply(tokenType(current))), insideMLComment)
      case (Nil, false) if tokenType(current).isRight => (tokens, insideMLComment)
      case (h :: t, false) if isMultiLineStart(current ++ h.toString) => tokenizeLine(h.toString, t, insideMLComment = true, tokens)
      case (h :: t, false) if isSignalSpaceIgnored(current) => tokenizeLine(h.toString, t, insideMLComment, tokens)
      case (h :: t, false) if isSignalLineIgnored(current ++ h.toString) => (tokens, insideMLComment)
      case (h :: t, false) if tokenType(current ++ h.toString).isLeft => tokenizeLine(current ++ h.toString, t, insideMLComment, tokens)
      case (h :: t, false) if tokenType(current).isLeft => tokenizeLine(h.toString, t, insideMLComment, Stream.concat(tokens, Stream.apply(tokenType(current))))
      case (h :: t, false) if tokenType(current).isRight => tokenizeLine(current ++ h.toString, t, insideMLComment, tokens)
      case (h :: t, false) => tokenizeLine(current ++ h.toString, t, insideMLComment, tokens)
      case (h :: t, true) if isMultiLineClose(current) => tokenizeLine(h.toString, t, insideMLComment = false, tokens)
      case (h :: t, true) => tokenizeLine(current ++ h.toString, t, insideMLComment, tokens)
    }

 def isMultiLineStart(input: String): Boolean = {
   val multiLineCommentPattern = "^/\\*.*$".r
   input match {
     case multiLineComment if multiLineCommentPattern.findFirstIn(input).isDefined => true
     case _ => false
   }
 }

 def isMultiLineClose(input: String): Boolean = {
   val multiLineCommentPattern = "^.*\\*/".r
   input match {
     case multiLineComment if multiLineCommentPattern.findFirstIn(input).isDefined => true
     case _ => false
   }
 }

  def isSignalSpaceIgnored(input: String): Boolean = {
    val whiteSpacePattern = "^\\s+$".r
    if (whiteSpacePattern.findFirstIn(input).isDefined) true else false
  }

 def isSignalLineIgnored(input: String): Boolean = {
    val commentPattern = "^\\s*//".r
    if (commentPattern.findFirstIn(input).isDefined) true else false
  }

  def tokenType(input: String): Either[Token, TokenizerError] = input match {
    case symbolToken if isSymbol(symbolToken) => getSymbolToken(SymbolToken.forName(symbolToken))
    case keywordToken if isKeyword(keywordToken) => getKeywordToken(KeywordToken.forName(keywordToken))
    case integerToken if isInteger(integerToken) => parseInteger(integerToken.toInt)
    case stringToken if isString(stringToken) => getStringToken(stringToken)
    case identifierToken if isIdentifier(identifierToken) => getIdentifierToken(identifierToken)
    case _ => Right(TokenizerError(s"$input is invalid token."))
  }

  private def isIdentifier(input: String) = {
    val identifierPattern = "^[a-zA-Z|_]+[a-zA-Z|0-9|_]*$".r //[(|\s+|;]
    if (identifierPattern.findFirstIn(input).toList.nonEmpty) true else false
  }

  private def getIdentifierToken(input: String) = {
    val identifierPattern = "^[a-zA-Z|_]+[a-zA-Z|0-9|_]*".r
    identifierPattern.findFirstIn(input) match {
      case Some(id) => Left(IdentifierToken(id))
      case None => Right(TokenizerError("Unable to find valid identifier."))
    }
  }

  private def isSymbol(input: String) = SymbolToken.forName(input).isDefined

  private def getSymbolToken(maybeSymbol: Option[SymbolValue]): Either[SymbolToken, TokenizerError] = {
    maybeSymbol match {
      case Some(symbol) => Left(tokens.SymbolToken(symbol))
      case None => Right(TokenizerError("Unable to find valid symbol."))
    }
  }

  private def isKeyword(input: String) = KeywordToken.forName(input).isDefined

  private def getKeywordToken(maybeKeyword: Option[KeywordValue]): Either[KeywordToken, TokenizerError] = {
    maybeKeyword match {
      case Some(keyword) => Left(tokens.KeywordToken(keyword))
      case None => Right(TokenizerError("Unable to find valid keyword."))
    }
  }

  private def isString(input: String) = {
    val stringPattern = "\".+\"".r
    if (stringPattern.findFirstIn(input).isDefined) true else false
  }

  private def getStringToken(input: String) = {
    val stringPattern = "\"(.*?)\"".r
    stringPattern.findFirstMatchIn(input) match {
      case Some(string) => Left(StringToken(string.group(1)))
      case None => Right(TokenizerError("Unable to find valid string."))
    }
  }

  private def isInteger(input: String) =
    try { input.toInt; true } catch { case e: NumberFormatException => false }

  private def parseInteger(int: Integer): Either[IntToken, TokenizerError] =
    if (int > -1 && int < 32768) Left(IntToken(int)) else Right(TokenizerError(s"Invalid integer: $int."))
}