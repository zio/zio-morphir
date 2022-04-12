package zio.morphir.ir.value.recursive

import zio.morphir.ir.{FQName, Literal => Lit, Name}

trait ValueConstructors {
  import Value._

  final def constructor[A](attributes: A, name: String): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor[A](attributes: A, name: FQName): Value[Nothing, A] = Constructor(attributes, name)
  final def constructor(name: String): RawValue                            = Constructor.Raw(name)
  final def constructor(name: FQName): RawValue                            = Constructor.Raw(name)

  final def fieldFunction[A](attributes: A, name: String): Value[Nothing, A] = FieldFunction(attributes, name)
  final def fieldFunction[A](attributes: A, name: Name): Value[Nothing, A]   = FieldFunction(attributes, name)
  final def fieldFunction(name: String): RawValue                            = FieldFunction.Raw(name)
  final def fieldFunction(name: Name): RawValue                              = FieldFunction.Raw(name)

  final def literal[VA, A](attributes: VA, literal: Lit[A]): Value[Nothing, VA] = Literal(attributes, literal)
  final def literal[A](literal: Lit[A]): RawValue                               = Literal.Raw(literal)

  final def reference[A](attributes: A, name: String): Value[Nothing, A] = Reference(attributes, name)
  final def reference[A](attributes: A, name: FQName): Value[Nothing, A] = Reference(attributes, name)
  final def reference(name: String): RawValue                            = Reference.Raw(name)
  final def reference(name: FQName): RawValue                            = Reference.Raw(name)

  final val unit: RawValue                            = Unit.Raw()
  final def unit[A](attributes: A): Value[Nothing, A] = Unit(attributes)

  final def variable[A](attributes: A, name: Name): Value[Nothing, A]   = Variable(attributes, name)
  final def variable[A](attributes: A, name: String): Value[Nothing, A] = Variable(attributes, name)
  final def variable(name: Name): RawValue                              = Variable.Raw(name)
  final def variable(name: String): RawValue                            = Variable.Raw(name)
}
