package kfulton.nand2tetris2.analyzer.tokenizer

trait Token

case class Keyword(keywordValue: KeywordValue) extends Token //set
case class Symbol(symbol: Char) extends Token
case class TokenInt(int: Int) extends Token //regex
case class TokenString(string: String) extends Token //regex
case class Identifier(alphaNumeric: String) extends Token //regex

sealed abstract class KeywordValue(name: String)
case object Class extends KeywordValue("class")