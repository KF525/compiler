package kfulton.nand2tetris2.analyzer.printer

import kfulton.nand2tetris2.analyzer.parser._

import scala.xml.{Elem, Node}
class XMLPrinter {

  def printGrammars(grammar: List[Jack], currentXML: Node = <tokens></tokens>): Node = grammar match {
    case Nil => currentXML
    case h::t => //h match {
      //case Left(token) =>
        val updatedNode = addChild(currentXML, printXML(h))
        printGrammars(t, updatedNode)
    //}
  }

  //At this point we know that everything is valid
  def printXML(jack: Jack): Node = ??? /*jack match {
    case JackClass(className, leftCurlyBrace, classVarDec, subroutineDec, rightCurlyBrace) =>
      val classVarDecNodes = classVarDec.map(printXML)
      val subRoutineNodes = subroutineDec.map(printXML)
      <class>
        <keyword>class</keyword>
        <identifier>{className.name}</identifier>
        <symbol>{leftCurlyBrace.symbolValue.canonical}</symbol>
        classVarDecNodes
        subRoutineNodes
        <symbol>{rightCurlyBrace.symbolValue.canonical}</symbol>
      </class>
    case ClassVarDec() => <tbd></tbd>
    case SubRoutineDec(dec, returnType, subRoutineName, leftParen, paramters, rightParen) =>
      val parameterList = printXML(parameterList)
      <subroutineDec>
        <keyword>dec</keyword>
        <identifier>returnType</identifier>
        <identifier>{subRoutineName.name}</identifier>
        <symbol>{leftParen.symbolValue.canonical}</symbol>
        <parameterList>
          parameterList
        </parameterList>
        <symbol>{rightParen.symbolValue.canonical}</symbol>
      </subroutineDec>
    case ParameterList(maybeParameter, additional) => <tbd></tbd>*/

  //}

  //jack match {
    //case LetStatement(varName: VarName, operation: Operation, expression: Expression) =>
//      //TODO: How do the opening and closing tags work for this?
//      printXML(varName)
//      printXML(operation)
//      printXML(expression)
//
//    //case KeywordToken(keywordValue: KeywordValue)=> <keyword>{keywordValue.canonical}</keyword>
//    //case SymbolToken(symbolValue)=> <symbol>{symbolValue.canonical}</symbol>
//    //case StringToken(string)=> <stringConstant>{string}</stringConstant>
//    //case IdentifierToken(string)=> <identifier>{string}</identifier>
//    case Operation(operator) => <symbol>{operator}</symbol> //TODO: I think I need canonical here as well
//    case VarName(name) => <identifier>{name}</identifier>  //TODO: Identifier versus String
//    case Constant(int) => <integerConstant>{int}</integerConstant>
  //}

  def saveXML(fileName: String, xml: Node) = scala.xml.XML.save(s"$fileName.xml", xml)

  //TODO: This is going to become more complex - currently it is printing flat
  private def addChild(n: Node, newChild: Node): Node =
    Elem.apply(n.prefix, n.label, n.attributes, n.scope, true, n.child ++ newChild : _*)
}