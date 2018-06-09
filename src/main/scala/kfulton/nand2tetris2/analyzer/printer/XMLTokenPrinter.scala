package kfulton.nand2tetris2.analyzer.printer

import kfulton.nand2tetris2.analyzer.tokenizer.tokens._

import scala.xml.{Elem, Node}
class XMLTokenPrinter {

  def printTokens(tokens: List[Token], fileName: String, currentXML: Node = <tokens></tokens>): Unit =
    tokens match {
      case Nil =>
        println(currentXML)
        saveXML(fileName, currentXML)
      case h::t =>
          val updatedNode = addChild(currentXML, printClass(h))
          printTokens(t, fileName, updatedNode)
    }

  def printClass(token: Token): Node = token match {
    case KeywordToken(k: KeywordValue) => <keyword>{k.canonical}</keyword>
    case IntToken(i: Int) => <integerConstant>{i}</integerConstant>
    case StringToken(s: String) => <stringConstant>{s}</stringConstant>
    case IdentifierToken(id : String) => <identifierToken>{id}</identifierToken>
    case SymbolToken(sy: SymbolValue) => <symbolToken>{sy.canonical}</symbolToken>
  }

  def saveXML(fileName: String, xml: Node): Unit = scala.xml.XML.save(s"$fileName.xml", xml)

  private def addChild(n: Node, newChild: Node): Node =
    Elem.apply(n.prefix, n.label, n.attributes, n.scope, true, n.child ++ newChild : _*)
}