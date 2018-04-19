package kfulton.nand2tetris2.analyzer.tokenizer

import kfulton.nand2tetris2.analyzer.tokenizer.tokens._

class Tokenizer {

  def advance(lines: Stream[String], tokens: Stream[Either[Token, TokenizerError]] = Stream.empty, insideMultiLineComment: Boolean = false):
  (Stream[Either[Token, TokenizerError]], Boolean) =
    lines.foldLeft((tokens, insideMultiLineComment)) {
      (currentTokens, line) =>
      val chars = line.toCharArray.toList
      val (newTokens, insideMLComment) = tokenizeLine(chars.head.toString, chars.tail, currentTokens._2, currentTokens._1)
      (Stream.concat(tokens, newTokens), insideMLComment)
    }

  def tokenizeLine(current: String, lines: List[Char], insideMLComment: Boolean, tokens: Stream[Either[Token, TokenizerError]] = Stream.empty):
  (Stream[Either[Token, TokenizerError]], Boolean) = (lines, insideMLComment) match {
      case (Nil, true) => (tokens, insideMLComment)
      case (Nil, false) if tokenType(current).isLeft => (Stream.concat(tokens, Stream.apply(tokenType(current))), insideMLComment)
      case (Nil, false) if tokenType(current).isRight => (tokens, insideMLComment)
      case (h :: t, false) if isMultiLineStart(current ++ h.toString) => tokenizeLine(h.toString, t, true, tokens)
      case (h :: t, false) if isSignalLineIgnored(current ++ h.toString) => (tokens, insideMLComment)
      case (h :: t, true) if isMultiLineClose(current) => tokenizeLine(h.toString, t, false, tokens)
      case (h :: t, false) if tokenType(current ++ h.toString).isLeft => tokenizeLine(current ++ h.toString, t, insideMLComment, tokens)
      case (h :: t, false) if tokenType(current).isLeft => tokenizeLine(h.toString, t, insideMLComment, Stream.concat(tokens, Stream.apply(tokenType(current))))
      case (h :: t, false) if tokenType(current).isRight => tokenizeLine(current ++ h.toString, t, insideMLComment, tokens)
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

 def isSignalLineIgnored(input: String): Boolean = {
    val commentPattern = "^\\s*//".r
    val whiteSpacePattern = "^\\s+$".r

    input match {
      case whiteSpace if whiteSpacePattern.findFirstIn(input).isDefined => true
      case comment if commentPattern.findFirstIn(input).isDefined => true
      case _ => false

    }
  }

  def tokenType(input: String): Either[Token, TokenizerError] = input match {
    case symbolToken if isSymbol(symbolToken) => getSymbolValue(SymbolToken.forName(symbolToken))
    case keywordToken if isKeyword(keywordToken) => getKeywordValue(KeywordToken.forName(keywordToken))
    case integer if isInteger(integer) => parseInteger(integer.toInt)
    //String and Identifier;
    case _ => Right(TokenizerError(s"$input is invalid token."))
  }

  private def isIdentifier(input: String) = {
    val identifierPattern = ".+(".r
    if (identifierPattern.findAllIn(input).toList.nonEmpty) true else false
  }

  private def isSymbol(input: String) = SymbolToken.forName(input).isDefined

  private def getSymbolValue(maybeSymbol: Option[SymbolValue]): Either[SymbolToken, TokenizerError] = {
    maybeSymbol match {
      case Some(symbol) => Left(tokens.SymbolToken(symbol))
      case None => Right(TokenizerError("Unable to find valid symbol."))
    }
  }

  private def isKeyword(input: String) = KeywordToken.forName(input).isDefined

  private def getKeywordValue(maybeKeyword: Option[KeywordValue]): Either[KeywordToken, TokenizerError] = {
    maybeKeyword match {
      case Some(keyword) => Left(tokens.KeywordToken(keyword))
      case None => Right(TokenizerError("Unable to find valid keyword."))
    }
  }

  private def isInteger(input: String) =
    try { input.toInt; true } catch { case e: NumberFormatException => false }

  private def parseInteger(int: Integer): Either[IntToken, TokenizerError] =
    if (int > -1 && int < 32768) Left(IntToken(int)) else Right(TokenizerError(s"Invalid integer: $int."))
}