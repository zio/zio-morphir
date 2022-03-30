package zio.morphir.ir.value

import zio.{Chunk}
import zio.prelude.AnyType
import zio.morphir.ir.ZEnvironmentSubset
import zio.morphir.ir.{Literal => Lit, _}
import zio.morphir.ir.value.Value.{Unit => UnitType, _}

trait ValueSyntax {

  def apply(function: RawValue, arguments: Chunk[RawValue]): RawValue = Apply.Raw(function, arguments)
  def apply(function: RawValue, arguments: RawValue*): RawValue = Apply.Raw(function, Chunk.fromIterable(arguments))

  def applyStrict[Caps[_]](function: TypedValue[Caps], arguments: Chunk[TypedValue[Caps]]): TypedValue[Caps] =
    Apply(function.attributes, function, arguments)

  def applyStrict[Caps[_]](function: TypedValue[Caps], arguments: TypedValue[Caps]*): TypedValue[Caps] =
    Apply(function.attributes, function, Chunk.fromIterable(arguments))

  final def boolean[Caps[_], Attributes](
      value: Boolean,
      attributes: ZEnvironmentSubset[Caps, Attributes]
  ): Value[Caps, Nothing, Attributes] =
    Literal(attributes, Lit.boolean(value))

  final def caseOf[Caps[_], TA, VA](value: Value[Caps, TA, VA])(
      cases: (Pattern[Caps, VA], Value[Caps, TA, VA])*
  ): Value[Caps, TA, VA] =
    PatternMatch(value.attributes, value, Chunk.fromIterable(cases))

  def constructor(name: FQName): RawValue = Constructor.Raw(name)
  def constructor[Caps[_], VA](attributes: ZEnvironmentSubset[Caps, VA], name: FQName): Value[Caps, Nothing, VA] =
    Constructor(attributes, name)

  final def boolean(value: Boolean): RawValue = literal(Lit.boolean(value))

  def destructure(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): Destructure.Raw =
    Destructure.Raw(pattern, valueToDestruct, inValue)

  def definition() = ???

  def field(tag: RawValue, name: Name): RawValue         = Field.Raw(tag, name)
  final def field(tag: RawValue, name: String): RawValue = Field.Raw(tag, Name.fromString(name))

  def fieldFunction(name: Name): RawValue = FieldFunction.Raw(name)

  def ifThenElse(condition: RawValue, thenBranch: RawValue, elseBranch: RawValue): RawValue =
    IfThenElse.Raw(condition, thenBranch, elseBranch)

  final def int(value: Int): RawValue = literal(Lit.int(value))

  final def lambda(pattern: UPattern, body: RawValue): RawValue =
    Lambda.Raw(pattern, body)

  def letDefinition(valueName: Name, valueDefinition: UDefinition, inValue: RawValue): RawValue =
    LetDefinition.Raw(valueName, valueDefinition, inValue)

  def letRecursion(valueDefinitions: Map[Name, UDefinition], inValue: RawValue): RawValue =
    LetRecursion.Raw(valueDefinitions, inValue)

  def list(elements: Chunk[RawValue]): RawValue = List.Raw(elements)
  def list(elements: RawValue*): RawValue       = List.Raw(Chunk.fromIterable(elements))
  def listOf[Caps[_]](elementType: UType)(elements: TypedValue[Caps]*)(implicit
      capabilities: Caps[UType]
  ): TypedValue[Caps] =
    List.Typed(elements: _*)(elementType)

  def literal[T](literal: Lit[T]): RawValue = Literal.Raw(literal)
  def literal(int: Int): RawValue           = Literal.Raw(Lit.int(int))
  def literal(string: String): RawValue     = Literal.Raw(Lit.string(string))
  def literal(boolean: Boolean): RawValue   = Literal.Raw(Lit.boolean(boolean))

  final def literal[V, Attributes](value: Lit[V])(attributes: ZEnvironmentSubset[AnyType, Attributes]): RawValue =
    Literal(attributes, value)

  def patternMatch(scrutinee: RawValue, cases: (UPattern, RawValue)*): RawValue =
    PatternMatch.Raw(scrutinee, Chunk.fromIterable(cases))

  def patternMatch(scrutinee: RawValue, cases: Chunk[(UPattern, RawValue)]): RawValue =
    PatternMatch.Raw(scrutinee, cases)

  def record(fields: (Name, RawValue)*): RawValue =
    Record.Raw(Chunk.fromIterable(fields))

  def record(fields: Chunk[(Name, RawValue)]): RawValue =
    Record.Raw(fields)

  def reference[Caps[_], VA](name: FQName, attributes: ZEnvironmentSubset[Caps, VA]): Value[Caps, Nothing, VA] =
    Reference(attributes, name)
  def reference(name: FQName): RawValue = Reference(name)
  def reference[Caps[_]](name: FQName, tpe: UType)(implicit capabilities: Caps[UType]): TypedValue[Caps] =
    Reference(ZEnvironmentSubset(tpe), name)

  final def string(value: String): RawValue = literal(Lit.string(value))
  final def string[Caps[_], Attributes](
      value: String,
      attributes: ZEnvironmentSubset[Caps, Attributes]
  ): Literal[Caps, Attributes, String] =
    Literal(attributes, Lit.string(value))

  def tuple(elements: Chunk[RawValue]): Tuple.Raw = Tuple.Raw(elements)
  def tuple(elements: RawValue*): Tuple.Raw       = Tuple.Raw(Chunk.fromIterable(elements))

  final val unit: RawValue = UnitType.Raw
  final def unit[Caps[_], Attributes](
      attributes: ZEnvironmentSubset[Caps, Attributes]
  ): Value[Caps, Nothing, Attributes] =
    UnitType(attributes)

  def updateRecord(valueToUpdate: RawValue, fieldsToUpdate: Chunk[(Name, RawValue)]): RawValue =
    UpdateRecord.Raw(valueToUpdate, fieldsToUpdate)

  final def variable(name: Name): RawValue = Variable.Raw(name)
  final def variable[Caps[_], A](name: Name, variableType: UType)(implicit
      capabilities: Caps[UType]
  ): TypedValue[Caps] =
    Variable(ZEnvironmentSubset(variableType), name)

  @inline final def variable(string: String): RawValue = variable(Name.fromString(string))

  @inline final def variable[Caps[_]](string: String, variableType: UType)(implicit
      capabilities: Caps[UType]
  ): TypedValue[Caps] =
    variable(Name.fromString(string), variableType)

  def wholeNumber(value: java.math.BigInteger): RawValue =
    literal(Lit.wholeNumber(value))

  @inline final val wildcardPattern: UPattern = Pattern.wildcardPattern
  @inline final def wildcardPattern[Attributes](
      attributes: ZEnvironmentSubset[AnyType, Attributes]
  ): Pattern[AnyType, Attributes] =
    Pattern.wildcardPattern(attributes)

  def asPattern(pattern: UPattern, name: Name): UPattern =
    Pattern.AsPattern(pattern, name, ZEnvironmentSubset.empty)

  def constructorPattern(name: FQName, patterns: Chunk[UPattern]): UPattern =
    Pattern.ConstructorPattern(name, patterns, ZEnvironmentSubset.empty)

  def emptyListPattern: UPattern = Pattern.EmptyListPattern(ZEnvironmentSubset.empty)

  def headTailPattern(head: UPattern, tail: UPattern): UPattern =
    Pattern.HeadTailPattern(head, tail, ZEnvironmentSubset.empty)

  def literalPattern[A](literal: Lit[A]): Pattern.LiteralPattern[AnyType, A, Any] =
    Pattern.LiteralPattern(literal, ZEnvironmentSubset.empty)

  def literalPattern(value: String): Pattern.LiteralPattern[AnyType, String, Any] =
    Pattern.LiteralPattern(Lit.string(value), ZEnvironmentSubset.empty)

  def literalPattern(int: Int): Pattern.LiteralPattern[AnyType, java.math.BigInteger, Any] =
    Pattern.LiteralPattern(Lit.int(int), ZEnvironmentSubset.empty)

  def literalPattern(boolean: Boolean): Pattern.LiteralPattern[AnyType, Boolean, Any] =
    Pattern.LiteralPattern(Lit.boolean(boolean), ZEnvironmentSubset.empty)

  def tuplePattern(patterns: UPattern*): UPattern =
    Pattern.TuplePattern(Chunk.fromIterable(patterns), ZEnvironmentSubset.empty)

  def nativeApply(function: NativeFunction, arguments: Chunk[RawValue]): RawValue =
    NativeApply(ZEnvironmentSubset.empty, function, arguments)

  def nativeApply(function: NativeFunction, arguments: RawValue*): RawValue =
    NativeApply(ZEnvironmentSubset.empty, function, Chunk.fromIterable(arguments))

  val unitPattern: UPattern = Pattern.UnitPattern(ZEnvironmentSubset.empty)

}

object ValueSyntax extends ValueSyntax
