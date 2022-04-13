package zio.morphir.ir.value.recursive

import zio.Chunk
import zio.morphir.ir.{FQName, IsNotAValue, Literal => Lit, Name}
import zio.morphir.ir.value.{Pattern, UPattern}

trait ValueConstructors {
  import Value._

  final def apply[TA, VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA]): Value[TA, VA] =
    Apply(attributes, function, argument)

  final def apply(function: RawValue, argument: RawValue): RawValue = Apply.Raw(function, argument)

  final def boolean[A](attributes: A, value: Boolean): Value[Nothing, A] = Literal(attributes, Lit.boolean(value))
  final def boolean(value: Boolean): RawValue                            = Literal.Raw(Lit.boolean(value))

  final def emptyTuple[VA](attributes: VA): Value[Nothing, VA] = Tuple(attributes)

  final def constructor[A](attributes: A, name: String): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor[A](attributes: A, name: FQName): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor(name: String): RawValue                            = Constructor.Raw(name)
  final def constructor(name: FQName): RawValue                            = Constructor.Raw(name)

  final def decimal[A](attributes: A, value: BigDecimal): Value[Nothing, A] = Literal(attributes, Lit.decimal(value))
  final def decimal(value: BigDecimal): RawValue                            = Literal.Raw(Lit.decimal(value))

  final def destructure[TA, VA](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
  ): Value[TA, VA] = Destructure(attributes, pattern, valueToDestruct, inValue)

  final def destructure(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): RawValue =
    Destructure.Raw(pattern, valueToDestruct, inValue)

  final def field[TA, VA](attributes: VA, target: Value[TA, VA], name: Name): Value[TA, VA] =
    Field(attributes, target, name)

  final def field[TA, VA](attributes: VA, target: Value[TA, VA], name: String): Value[TA, VA] =
    Field(attributes, target, name)

  final def field(target: RawValue, name: Name): RawValue = Field.Raw(target, name)

  final def field(target: RawValue, name: String): RawValue = Field.Raw(target, name)

  final def fieldFunction[A](attributes: A, name: String): Value[Nothing, A] = FieldFunction(attributes, name)
  final def fieldFunction[A](attributes: A, name: Name): Value[Nothing, A]   = FieldFunction(attributes, name)
  final def fieldFunction(name: String): RawValue                            = FieldFunction.Raw(name)
  final def fieldFunction(name: Name): RawValue                              = FieldFunction.Raw(name)

  final def float[A](attributes: A, value: Float): Value[Nothing, A] = Literal(attributes, Lit.float(value))
  final def float(value: Float): RawValue                            = Literal.Raw(Lit.float(value))

  final def int[A](attributes: A, value: Int): Value[Nothing, A] = Literal(attributes, Lit.int(value))
  final def int(value: Int): RawValue                            = Literal.Raw(Lit.int(value))

  final def lambda[TA, VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA]): Value[TA, VA] =
    Lambda(attributes, argumentPattern, body)

  final def lambda(argumentPattern: UPattern, body: RawValue): RawValue = Lambda.Raw(argumentPattern, body)

  final def letDestruct[TA, VA](
      attributes: VA,
      pattern: Pattern[VA],
      valueToDestruct: Value[TA, VA],
      inValue: Value[TA, VA]
  ): Value[TA, VA] = Destructure(attributes, pattern, valueToDestruct, inValue)

  final def letDestruct(pattern: UPattern, valueToDestruct: RawValue, inValue: RawValue): RawValue =
    Destructure.Raw(pattern, valueToDestruct, inValue)

  final def list[TA, VA](attributes: VA, values: Chunk[Value[TA, VA]]): Value[TA, VA] =
    List(attributes, values)

  final def list[TA, VA](attributes: VA, values: Value[TA, VA]*)(implicit ev: IsNotAValue[VA]): Value[TA, VA] =
    List(attributes, values: _*)

  final def list(elements: Chunk[RawValue]): RawValue = List.Raw(elements)
  final def list(elements: RawValue*): RawValue       = List.Raw(elements: _*)

  final def literal[VA, A](attributes: VA, literal: Lit[A]): Value[Nothing, VA] = Literal(attributes, literal)
  final def literal[A](literal: Lit[A]): RawValue                               = Literal.Raw(literal)

  final def long[A](attributes: A, value: Long): Value[Nothing, A] = Literal(attributes, Lit.long(value))
  final def long(value: Long): RawValue                            = Literal.Raw(Lit.long(value))

  final def record[TA, VA](attributes: VA, fields: Chunk[(Name, Value[TA, VA])]): Value[TA, VA] =
    Record(attributes, fields)

  final def record[TA, VA](attributes: VA, fields: (String, Value[TA, VA])*)(implicit
      ev: IsNotAValue[VA]
  ): Value[TA, VA] = Record(attributes, fields: _*)

  final def record(fields: Chunk[(Name, RawValue)]): RawValue = Record.Raw(fields)
  final def record(fields: (String, RawValue)*): RawValue     = Record.Raw(fields: _*)
  final def record(firstField: (Name, RawValue), otherFields: (Name, RawValue)*): RawValue =
    Record.Raw(firstField +: Chunk.fromIterable(otherFields))

  final def reference[A](attributes: A, name: String): Value[Nothing, A] = Reference(attributes, name)
  final def reference[A](attributes: A, name: FQName): Value[Nothing, A] = Reference(attributes, name)
  final def reference[A](attributes: A, packageName: String, moduleName: String, localName: String): Value[Nothing, A] =
    Reference(attributes, packageName, moduleName, localName)
  final def reference(name: String): RawValue = Reference.Raw(name)
  final def reference(name: FQName): RawValue = Reference.Raw(name)
  final def reference(packageName: String, moduleName: String, localName: String): RawValue =
    Reference.Raw(packageName, moduleName, localName)

  final def string[VA](attributes: VA, value: String): Value[Nothing, VA] = Literal(attributes, Lit.string(value))
  final def string(value: String): RawValue                               = Literal.Raw(Lit.string(value))

  final def tuple[TA, VA](attributes: VA, elements: Chunk[Value[TA, VA]]): Value[TA, VA] = Tuple(attributes, elements)
  final def tuple[TA, VA](attributes: VA, first: Value[TA, VA], second: Value[TA, VA], otherElements: Value[TA, VA]*)(
      implicit ev: IsNotAValue[VA]
  ): Value[TA, VA] = Tuple(attributes, first +: second +: Chunk.fromIterable(otherElements))

  final def tuple(elements: RawValue*): RawValue = Tuple.Raw(elements: _*)

  final val unit: RawValue                            = Unit.Raw()
  final def unit[A](attributes: A): Value[Nothing, A] = Unit(attributes)

  final def variable[A](attributes: A, name: Name): Value[Nothing, A]   = Variable(attributes, name)
  final def variable[A](attributes: A, name: String): Value[Nothing, A] = Variable(attributes, name)
  final def variable(name: Name): RawValue                              = Variable.Raw(name)
  final def variable(name: String): RawValue                            = Variable.Raw(name)
}
