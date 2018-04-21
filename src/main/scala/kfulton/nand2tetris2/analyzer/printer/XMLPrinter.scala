package kfulton.nand2tetris2.analyzer.printer

import kfulton.nand2tetris2.analyzer.tokenizer.tokens._

import scala.xml.{Elem, Node}
class XMLPrinter {

  def printTokens(tokens: Stream[Either[Token, TokenizerError]], currentXML: Node = <tokens></tokens>): Node = tokens match {
    case Stream.Empty => currentXML
    case h #:: t => h match {
      case Left(token) =>
        val updatedNode = addChild(currentXML, printToken(token))
        printTokens(t, updatedNode)
    }

  }

  def printToken(token: Token): Node =  token match {
    case KeywordToken(keywordValue: KeywordValue)=> <keyword>{keywordValue.canonical}</keyword>
    case SymbolToken(symbolValue)=> <symbol>{symbolValue.canonical}</symbol>
    case StringToken(string)=> <stringConstant>{string}</stringConstant>
    case IdentifierToken(string)=> <identifier>{string}</identifier>
    case IntToken(int)=> <integerConstant>{int}</integerConstant>
  }

  def saveXML(fileName: String, xml: Node) = scala.xml.XML.save(s"$fileName.xml", xml)

  //TODO: This is going to become more complex - currently it is printing flat
  private def addChild(n: Node, newChild: Node): Node =
    Elem.apply(n.prefix, n.label, n.attributes, n.scope, true, n.child ++ newChild : _*)
}