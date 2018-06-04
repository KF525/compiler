package kfulton.nand2tetris2.analyzer

import java.io.File

import kfulton.nand2tetris2.analyzer.printer.XMLPrinter
import kfulton.nand2tetris2.analyzer.tokenizer.Tokenizer
import kfulton.nand2tetris2.analyzer.tokenizer.tokens.{Token, TokenizerError}
import kfulton.nand2tetris2.analyzer.parser.JParser
import scala.io.Source

class JackAnalyzer {
  val tokenizer = new Tokenizer
  val parser = JParser
  val printer = new XMLPrinter

  def runTokenizer(path: String) = {
    val eitherTokens: List[Either[Token, TokenizerError]] = tokenizer.advance(createList(path))
    val (tokens, failure) = validateTokenStream(eitherTokens)
    if (failure.isEmpty) 3//then parse


    //val xml = parser.parseTokens(tokens)
  }

  def validateTokenStream(eitherTokens: List[Either[Token, TokenizerError]],
                          tokens: List[Token] = Nil,
                          failure: Option[TokenizerError] = None): (List[Token], Option[TokenizerError]) =
    (eitherTokens, failure) match {
      case (_, Some(tokenizerError)) => (tokens, Some(tokenizerError))
      case (Left(token) :: t, None) => validateTokenStream(t, token :: tokens, None)
      case (Right(tokenizerError) :: t, None) => validateTokenStream(t, tokens, Some(tokenizerError))
    }

  def createList(path: String): List[String] = {
    val file = new File(path)
    Source.fromFile(file).getLines().toList
  }
}
