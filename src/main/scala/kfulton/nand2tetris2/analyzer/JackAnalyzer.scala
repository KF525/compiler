package kfulton.nand2tetris2.analyzer

import java.io.File

import kfulton.nand2tetris2.analyzer.tokenizer.Tokenizer

import scala.io.Source

class JackAnalyzer {
  val tokenizer = new Tokenizer

  def runTokenizer(path: String) = {
    val tokens = tokenizer.advance(createStream(path))
  }

  def createStream(path: String): Stream[String] = {
    val file = new File(path)
    Source.fromFile(file).getLines().toStream
  }
}
