package kfulton.nand2tetris2.analyzer.tokenizer.tokens

trait Token

case class KeywordToken(keywordValue: KeywordValue) extends Token
case class SymbolToken(symbolValue: SymbolValue) extends Token
case class IntToken(int: Int) extends Token
case class StringToken(string: String) extends Token
case class IdentifierToken(alphaNumeric: String) extends Token

object SymbolToken extends NamedEnum[SymbolValue] {
  override def values: Vector[SymbolValue] =
    Vector(LeftParen, RightParen, LeftCurlyBracket, RightCurlyBracket, LeftSquareBracket,
      RightSquareBracket, Period, Comma, SemiColon, Plus, Minus, Asterisk, Slash, Amp,
      Pipe, LessThan, GreaterThan, Equal, Dash)
}

trait SymbolValue extends CanonicalName
case object LeftCurlyBracket extends SymbolValue { val canonical: String = "{" }
case object RightCurlyBracket extends SymbolValue { val canonical: String = "}" }
case object LeftParen extends SymbolValue { val canonical: String = "(" }
case object RightParen extends SymbolValue { val canonical: String = ")" }
case object LeftSquareBracket extends SymbolValue { val canonical: String = "[" }
case object RightSquareBracket extends SymbolValue { val canonical: String = "]" }
case object Period extends SymbolValue { val canonical: String = "." }
case object Comma extends SymbolValue { val canonical: String = "," }
case object SemiColon extends SymbolValue { val canonical: String = ";" }
case object Plus extends SymbolValue { val canonical: String = "+" }
case object Minus extends SymbolValue { val canonical: String = "-" }
case object Asterisk extends SymbolValue { val canonical: String = "*" }
case object Slash extends SymbolValue { val canonical: String = "/" }
case object Amp extends SymbolValue { val canonical: String = "&" }
case object Pipe extends SymbolValue { val canonical: String = "|" }
case object LessThan extends SymbolValue { val canonical: String = "<" }
case object GreaterThan extends SymbolValue { val canonical: String = ">" }
case object Equal extends SymbolValue { val canonical: String = "=" }
case object Dash extends SymbolValue { val canonical: String = "-" }


object KeywordToken extends NamedEnum[KeywordValue] {
  override def values: Vector[KeywordValue] =
    Vector(Class, Constructor, Function, Method, Field, Static, Var, True,
      False, Null, This, Let, Do, If, Else, While, Return)
}
trait KeywordValue extends CanonicalName
case object Class extends KeywordValue { val canonical: String = "class" }
case object Constructor extends KeywordValue { val canonical: String = "constructor" }
case object Function extends KeywordValue { val canonical: String = "function" }
case object Method extends KeywordValue { val canonical: String = "method" }
case object Field extends KeywordValue { val canonical: String = "field" }
case object Static extends KeywordValue { val canonical: String = "static" }
case object Var extends KeywordValue { val canonical: String = "var" }
case object True extends KeywordValue { val canonical: String = "true" }
case object False extends KeywordValue { val canonical: String = "false" }
case object Null extends KeywordValue { val canonical: String = "null" }
case object This extends KeywordValue { val canonical: String = "this" }
case object Let extends KeywordValue { val canonical: String = "let" }
case object Do extends KeywordValue { val canonical: String = "do" }
case object If extends KeywordValue { val canonical: String = "if" }
case object Else extends KeywordValue { val canonical: String = "else" }
case object While extends KeywordValue { val canonical: String = "while" }
case object Return extends KeywordValue { val canonical: String = "return" }

trait CanonicalName {
  val canonical: String
}

trait NamedEnum[T <: CanonicalName] {
  def values: Vector[T]

  def forName(name: String): Option[T] =
    values.find(_.canonical.equalsIgnoreCase(name.trim))
}