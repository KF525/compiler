package kfulton.nand2tetris2.analyzer.tokenizer.printer

import kfulton.nand2tetris2.analyzer.tokenizer.tokens._

import scala.xml.{Elem, Node}
class XMLPrinter {

  def printTokens(tokens: Stream[Token], currentXML: Node = <tokens></tokens>): Node = tokens match {
    case Stream.Empty => currentXML
    case h #:: t =>
      val updatedNode = addChild(currentXML, printToken(h))
      printTokens(t, updatedNode)
  }

  def printToken(token: Token): Node =  token match {
    case KeywordToken(keywordValue: KeywordValue)=> <keyword>{keywordValue.canonical}</keyword>
    case SymbolToken(symbolValue)=> <symbol>{symbolValue.canonical}</symbol>
    case StringToken(string)=> <stringConstant>{string}</stringConstant>
    case IdentifierToken(string)=> <identifier>{string}</identifier>
    case IntToken(int)=> <integerConstant>{int}</integerConstant>
  }

  def saveXML(fileName: String, xml: Node) = scala.xml.XML.save(s"$fileName.xml", xml)

  private def addChild(n: Node, newChild: Node): Node =
    Elem.apply(n.prefix, n.label, n.attributes, n.scope, true, n.child ++ newChild : _*)
}