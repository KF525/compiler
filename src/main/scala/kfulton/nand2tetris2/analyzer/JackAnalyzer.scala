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

  def runAnalyzer(path: String) = {
    val eitherTokens: List[Either[Token, TokenizerError]] = tokenizer.advance(getProgram(path))
    val (tokens, failure) = validateTokens(eitherTokens)
    parser.parseJProgram(tokens) match {
      case Left(e) =>
      case Right(result) => printer.printGrammars(result._2)
    }
  }

  def validateTokens(eitherTokens: List[Either[Token, TokenizerError]],
                     tokens: List[Token] = List(),
                     failure: Option[TokenizerError] = None): (List[Token], Option[TokenizerError]) =
    (eitherTokens, failure) match {
      case (Nil, None) => (tokens, None)
      case (_, Some(tokenizerError)) => (tokens, Some(tokenizerError))
      case (Left(token) :: t, None) => validateTokens(t, tokens :+ token, None)
      case (Right(tokenizerError) :: t, None) => validateTokens(t, tokens, Some(tokenizerError))
    }

  def getProgram(path: String): List[String] = {
    val file = new File(path)
    Source.fromFile(file).getLines().toList
  }
}
