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
    val eitherTokens: Stream[Either[Token, TokenizerError]] = tokenizer.advance(createStream(path))
    val (tokens, failure) = validateTokenStream(eitherTokens)
    if (failure.isEmpty) 3//then parse


    //val xml = parser.parseTokens(tokens)
  }

  def validateTokenStream(eitherTokens: Stream[Either[Token, TokenizerError]],
                          tokens: Stream[Token] = Stream.Empty,
                          failure: Option[TokenizerError] = None): (Stream[Token], Option[TokenizerError]) =
    (eitherTokens, failure) match {
      case (_, Some(tokenizerError)) => (tokens, Some(tokenizerError))
      case (Left(token) #:: t, None) => validateTokenStream(t, token #:: tokens, None)
      case (Right(tokenizerError) #:: t, None) => validateTokenStream(t, tokens, Some(tokenizerError))
    }

  def createStream(path: String): Stream[String] = {
    val file = new File(path)
    Source.fromFile(file).getLines().toStream
  }
}
