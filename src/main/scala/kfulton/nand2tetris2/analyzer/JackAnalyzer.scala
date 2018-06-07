package kfulton.nand2tetris2.analyzer

import java.io.File

import kfulton.nand2tetris2.analyzer.parser.JParser
import kfulton.nand2tetris2.analyzer.printer.XMLPrinter
import kfulton.nand2tetris2.analyzer.tokenizer.Tokenizer
import kfulton.nand2tetris2.analyzer.tokenizer.tokens.{Token, TokenizerError}

import scala.io.Source

class JackAnalyzer {
  val tokenizer = new Tokenizer
  val parser = JParser
  val printer = new XMLPrinter

  def runTokenizer(path: String) = {
    val eitherTokens: List[Either[Token, TokenizerError]] = tokenizer.advance(program(path))
    val (tokens, failure) = validateTokens(eitherTokens)

   // val x = if (failure.isEmpty) parser.parseJProgram(tokens) else



    //val xml = parser.parseTokens(tokens)
  }

  def validateTokens(eitherTokens: List[Either[Token, TokenizerError]],
                     tokens: List[Token] = List(),
                     failure: Option[TokenizerError] = None): (List[Token], Option[TokenizerError]) =
    (eitherTokens, failure) match {
      case (_, Some(tokenizerError)) => (tokens, Some(tokenizerError))
      case (Left(token) :: t, None) => validateTokens(t, token :: tokens, None)
      case (Right(tokenizerError) :: t, None) => validateTokens(t, tokens, Some(tokenizerError))
    }

  def program(path: String): List[String] = {
    val file = new File(path)
    Source.fromFile(file).getLines().toList
  }
}
