package kfulton.nand2tetris2.analyzer

import java.io.File

import kfulton.nand2tetris2.analyzer.parser.Parser
import kfulton.nand2tetris2.analyzer.printer.XMLPrinter
import kfulton.nand2tetris2.analyzer.tokenizer.Tokenizer
import kfulton.nand2tetris2.analyzer.tokenizer.tokens.{Token, TokenizerError}

import scala.io.Source

class JackAnalyzer {
  val tokenizer = new Tokenizer
  val parser = new Parser
  val printer = new XMLPrinter

  def runTokenizer(path: String) = {
    val tokens = tokenizer.advance(createStream(path))
    val xml = parser.parseTokens(tokens)
  }

  def createStream(path: String): Stream[String] = {
    val file = new File(path)
    Source.fromFile(file).getLines().toStream
  }
}
