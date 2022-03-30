package zio.morphir.ir.value

import zio.Chunk
import zio.morphir.ir.{Literal => Lit, _}
import zio.morphir.ir.value.Value.{Unit => UnitType, _}

trait ValueSyntax {

  def apply(function: RawValue, arguments: Chunk[RawValue]): RawValue = Apply.Raw(function, arguments)
  def apply(function: RawValue, arguments: RawValue*): RawValue = Apply.Raw(function, Chunk.fromIterable(arguments))

  def applyStrict(function: TypedValue, arguments: Chunk[TypedValue]): TypedValue =
    Apply(function.attributes, function, arguments)

  def applyStrict(function: TypedValue, arguments: TypedValue*): TypedValue =
    Apply(function.attributes, function, Chunk.fromIterable(arguments))

  final def boolean[Attributes](value: Boolean, attributes: Attributes): Value[Nothing, Attributes] =
    Literal(attributes, Lit.boolean(value))

  final def caseOf[TA, VA](value: Value[TA, VA])(cases: (Pattern[VA], Value[TA, VA])*): Value[TA, VA] =
    PatternMatch(value.attributes, value, Chunk.fromIterable(cases))

  def constructor(name: FQName): RawValue = Constructor.Raw(name)
  def constructor[VA](attributes: VA, name: FQName): Value[Nothing, VA] =
    Constructor(attributes, name)

  final def boolean(value: Boolean): RawValue = literal(Lit.boolean(value))

  def destructure(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): Destructure.Raw =
    Destructure(pattern, valueToDestruct, inValue)

  def definition() = ???

  def field(tag: RawValue, name: Name): RawValue         = Field.Raw(tag, name)
  final def field(tag: RawValue, name: String): RawValue = Field.Raw(tag, Name.fromString(name))

  def fieldFunction(name: Name): RawValue = FieldFunction.Raw(name)

  def ifThenElse(condition: RawValue, thenBranch: RawValue, elseBranch: RawValue): RawValue =
    IfThenElse(condition, thenBranch, elseBranch)

  final def int(value: Int): RawValue = literal(Lit.int(value))

  final def lambda(pattern: UPattern, body: RawValue): RawValue =
    Lambda.Raw(pattern, body)

  def letDefinition(valueName: Name, valueDefinition: UDefinition, inValue: RawValue): RawValue =
    LetDefinition(valueName, valueDefinition, inValue)

  def letRecursion(valueDefinitions: Map[Name, UDefinition], inValue: RawValue): RawValue =
    LetRecursion.Raw(valueDefinitions, inValue)

  def list(elements: Chunk[RawValue]): RawValue                     = List.Raw(elements)
  def list(elements: RawValue*): RawValue                           = List.Raw(Chunk.fromIterable(elements))
  def listOf(elementType: UType)(elements: TypedValue*): TypedValue = List.Typed(elements: _*)(elementType)

  def literal[T](literal: Lit[T]): RawValue = Literal.Raw(literal)
  def literal(int: Int): RawValue           = Literal.Raw(Lit.int(int))
  def literal(string: String): RawValue     = Literal.Raw(Lit.string(string))
  def literal(boolean: Boolean): RawValue   = Literal.Raw(Lit.boolean(boolean))

  final def literal[V, Attributes](value: Lit[V])(attributes: Attributes): RawValue =
    Literal(attributes, value)

  def patternMatch(scrutinee: RawValue, cases: (UPattern, RawValue)*): RawValue =
    PatternMatch.Raw(scrutinee, Chunk.fromIterable(cases))

  def patternMatch(scrutinee: RawValue, cases: Chunk[(UPattern, RawValue)]): RawValue =
    PatternMatch.Raw(scrutinee, cases)

  def record(fields: (Name, RawValue)*): RawValue =
    Record.Raw(Chunk.fromIterable(fields))

  def record(fields: Chunk[(Name, RawValue)]): RawValue =
    Record.Raw(fields)

  def reference[VA](name: FQName, attributes: VA): Value[Nothing, VA] = Reference(attributes, name)
  def reference(name: FQName): RawValue                               = Reference(name)
  def reference(name: FQName, tpe: UType): TypedValue                 = Reference(tpe, name)

  final def string(value: String): RawValue = literal(Lit.string(value))
  final def string[Attributes](value: String, attributes: Attributes): Literal[Attributes, String] =
    Literal(attributes, Lit.string(value))

  def tuple(elements: Chunk[RawValue]): Tuple.Raw = Tuple.Raw(elements)
  def tuple(elements: RawValue*): Tuple.Raw       = Tuple.Raw(Chunk.fromIterable(elements))

  final val unit: RawValue                                                       = UnitType.Raw
  final def unit[Attributes](attributes: Attributes): Value[Nothing, Attributes] = UnitType(attributes)

  def updateRecord(valueToUpdate: RawValue, fieldsToUpdate: Chunk[(Name, RawValue)]): RawValue =
    UpdateRecord.Raw(valueToUpdate, fieldsToUpdate)

  final def variable(name: Name): RawValue                           = Variable.Raw(name)
  final def variable[A](name: Name, variableType: UType): TypedValue = Variable(variableType, name)

  @inline final def variable(string: String): RawValue = variable(Name.fromString(string))

  @inline final def variable[A](string: String, variableType: UType): TypedValue =
    variable(Name.fromString(string), variableType)

  def wholeNumber(value: java.math.BigInteger): RawValue =
    literal(Lit.wholeNumber(value))

  @inline final val wildcardPattern: UPattern = Pattern.wildcardPattern
  @inline final def wildcardPattern[Attributes](
      attributes: Attributes
  ): Pattern[Attributes] =
    Pattern.wildcardPattern(attributes)

  def asPattern(pattern: UPattern, name: Name): UPattern =
    Pattern.AsPattern(pattern, name, ())

  def constructorPattern(name: FQName, patterns: Chunk[UPattern]): UPattern =
    Pattern.ConstructorPattern(name, patterns, ())

  def emptyListPattern: UPattern = Pattern.EmptyListPattern(())

  def headTailPattern(head: UPattern, tail: UPattern): UPattern =
    Pattern.HeadTailPattern(head, tail, ())

  def literalPattern[A](literal: Lit[A]): Pattern.LiteralPattern[A, Any] =
    Pattern.LiteralPattern(literal, ())

  def literalPattern(value: String): Pattern.LiteralPattern[String, Any] =
    Pattern.LiteralPattern(Lit.string(value), ())

  def literalPattern(int: Int): Pattern.LiteralPattern[java.math.BigInteger, Any] =
    Pattern.LiteralPattern(Lit.int(int), ())

  def literalPattern(boolean: Boolean): Pattern.LiteralPattern[Boolean, Any] =
    Pattern.LiteralPattern(Lit.boolean(boolean), ())

  def tuplePattern(patterns: UPattern*): UPattern =
    Pattern.TuplePattern(Chunk.fromIterable(patterns), ())

  def nativeApply(function: NativeFunction, arguments: Chunk[RawValue]): RawValue =
    NativeApply((), function, arguments)

  def nativeApply(function: NativeFunction, arguments: RawValue*): RawValue =
    NativeApply((), function, Chunk.fromIterable(arguments))

  val unitPattern: UPattern = Pattern.UnitPattern(())

}

object ValueSyntax extends ValueSyntax
