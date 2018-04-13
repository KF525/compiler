package kfulton.nand2tetris2.analyzer.tokenizer

trait Token

case class Keyword(keywordValue: KeywordValue) extends Token //set
case class Symbol(symbol: Char) extends Token
case class TokenInt(int: Int) extends Token //regex
case class TokenString(string: String) extends Token //regex
case class Identifier(alphaNumeric: String) extends Token //regex

sealed abstract class KeywordValue extends CanonicalName {
  def values = Vector(Class)
}

case object Class extends KeywordValue {
  override val canonical: String = "class"
}

trait CanonicalName {
  val canonical: String
}

trait NamedEnum[T <: CanonicalName] {
  def values: Vector[T]

  def forName(name: String): Option[T] =
    values.find(_.canonical.equalsIgnoreCase(name.trim))
}

