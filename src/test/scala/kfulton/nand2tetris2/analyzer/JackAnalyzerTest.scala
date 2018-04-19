package kfulton.nand2tetris2.analyzer

import org.scalatest.{FlatSpec, Matchers}

class JackAnalyzerTest extends FlatSpec with Matchers {
  val jackAnalyzer = new JackAnalyzer

  "runTokenizer" should "" in {
    pending
    //val x = jackAnalyzer.runTokenizer("/Users/katefulton/Desktop/N2T/compiler/src/main/resources/simpletest.jack")
  }

  "createStream" should "" in {
    val stream = jackAnalyzer.createStream("/Users/katefulton/Desktop/N2T/compiler/src/main/resources/simpletest.jack")
    stream shouldBe Stream("class Point {", "", "}")
  }
}
