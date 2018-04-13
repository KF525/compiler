package kfulton.nand2tetris2.analyzer.tokenizer

class Tokenizer {
  def hasMoreTokens = ???

  def advance = ???

  def tokenType(token: String): Either[Token, TokenizerError] = token match {
    case symbolToken if isSymbol(symbolToken) => Left(Symbol(symbolToken.charAt(0)))
    case keywordToken if isKeyword(keywordToken) => Left(Keyword(getKeywordValue(keywordToken)))
    case integer if parseInt(integer).isDefined => ???
    case _ => Right(TokenizerError(s"$token is invalid token."))
  }

  //TODO: Better way to do this all with enums?
  private def isSymbol(token: String) =
      Set("(", ")").contains(token)

  private def isKeyword(token: String) =
      Set("class").contains(token)

  //parse and check range
  private def parseInt(token: String): Option[Int] =
    try {
      val int = token.toInt
      if (int > -1 && int < 32768) Some(int) else None
    } catch {
      case e: NumberFormatException => None
    }

  private def getKeywordValue(token: String): KeywordValue = token match {
    case classToken if token.equals("class") => Class
  }
}
