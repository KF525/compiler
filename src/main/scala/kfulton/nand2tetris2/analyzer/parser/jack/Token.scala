package kfulton.nand2tetris2.analyzer.parser.jack

import kfulton.nand2tetris2.analyzer.parser._

case class JackAST(classes: List[JClass])

case class JackError(message: String)
trait Token

case class JClass(jName: JName, jClassVarDecList: List[JClassVarDec],
                  jSubRoutineDecList: List[JSubRoutineDec]) extends Token
case class JSubRoutineBody(jVarDecList: List[JVarDec], statements: JStatements) extends Token
case class JSubRoutineDec(jSubRoutineType: JSubRoutineType, jReturnType: JReturnType, jSubroutineName: JName,
                          parameterList: List[JParameter], jSubRoutineBody: JSubRoutineBody) extends Token

case class JName(name: String) extends Token
case class JVoid() extends Token
case class JReturnType(voidOrType: Either[JVoid, JType]) extends Token

sealed trait JSubRoutineType extends Token
case object JConstructor extends JSubRoutineType
case object JMethod extends JSubRoutineType
case object JFunction extends JSubRoutineType

case class JClassVarDec(jClassVar: JClassVar, jType: JType, jName: JName, additionalJVar: List[JName]) extends Token
case class JVarDec(jType: JType, jName: JName, additionalJVar: List[JName] = Nil) extends Token

sealed trait JClassVar extends Token
case object JStatic extends JClassVar
case object JField extends JClassVar

case class JType(jType: JPrimitiveType) extends Token
sealed trait JPrimitiveType extends Token
case object JIntPrimitiveType extends JPrimitiveType
case object JCharPrimitiveType extends JPrimitiveType
case object JBooleanPrimitiveType extends JPrimitiveType
case class JClassNameType(id: String) extends JPrimitiveType

case class JParameter(jType: JType, jName: JName)
case class JParameterList(jParameters: List[JParameter]) extends Token
case class JExpression(term: JTerm, additional: List[JOpTerm]) extends Token
case class JExpressionList(jexpression: List[JExpression]) extends Token

case class JSubRoutineCall(subRoutineCallType: JSubRoutineCallType) extends Token
sealed trait JSubRoutineCallType extends Token
case class JBareSubRoutineCall(jName: JName, jExpressionList: List[JExpression]) extends JSubRoutineCallType
case class JClassSubroutineCall(jName: JName, subroutineName: JName,
                                expressionList: List[JExpression]) extends JSubRoutineCallType

case class JOpTerm(op: JOp, term: JTerm)

sealed trait JTerm extends Token
case class JIntegerTerm(int: Int) extends JTerm
case class JStringTerm(str: String) extends JTerm
case class JKeywordTerm(k: JKeywordConstant) extends JTerm
case class JVarNameWithOptionalExpressionTerm(jName: JName, optionJExpression: Option[JExpression]) extends JTerm
case class JExpressionTerm(jExpression: JExpression) extends JTerm
case class JSubRoutineExpressionTerm(jSubRoutineCallType: JSubRoutineCallType) extends JTerm
case class JUnaryOpTerm(jUnaryOp: JUnaryOp, jTerm: JTerm) extends JTerm

sealed trait JOp extends Token
case object JPlus extends JOp
case object JMinus extends JOp
case object JAsterisk extends JOp
case object JSlash extends JOp
case object JAmp extends JOp
case object JPipe extends JOp
case object JLessThan extends JOp
case object JGreaterThan extends JOp
case object JEqual extends JOp

sealed trait JUnaryOp extends Token
case object JDash extends JUnaryOp
case object JTilda extends JUnaryOp

object JKeywordTerm extends NamedEnum[JKeywordConstant] {
  override def values: Vector[JKeywordConstant] =
    Vector(JTrue, JFalse, JNull, JThis)
}

sealed trait JKeywordConstant extends Token with CanonicalName
case object JTrue extends JKeywordConstant {override val canonical: String = "true"}
case object JFalse extends JKeywordConstant {override val canonical: String = "false"}
case object JNull extends JKeywordConstant {override val canonical: String = "null"}
case object JThis extends JKeywordConstant {override val canonical: String = "this"}

case class JStatements(jStatement: List[JStatement]) extends Token
sealed trait JStatement extends Token
case class JLetStatement(jName: JName, maybeJExpression: Option[JExpression], jExpression: JExpression) extends JStatement
case class JIfStatement(jExpression: JExpression, jStatements1: JStatements, maybeJStatements2: Option[JStatements]) extends JStatement
case class JWhileStatement(jExpression: JExpression, jStatements: JStatements) extends JStatement
case class JDoStatement(jSubRoutineCall: JSubRoutineCall) extends JStatement
case class JReturnStatement(jExpression: JExpression) extends JStatement