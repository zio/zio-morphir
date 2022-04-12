package zio.morphir.ir.value.recursive

import zio.morphir.ir.Name

trait ValueConstructors {
  import Value._

  final def fieldFunction[A](attributes: A, name: String): Value[Nothing, A] = FieldFunction(attributes, name)
  final def fieldFunction[A](attributes: A, name: Name): Value[Nothing, A]   = FieldFunction(attributes, name)
  final def fieldFunction(name: String): RawValue                            = FieldFunction.Raw(name)
  final def fieldFunction(name: Name): RawValue                              = FieldFunction.Raw(name)

  final val unit: RawValue                            = Unit.Raw()
  final def unit[A](attributes: A): Value[Nothing, A] = Unit(attributes)

  final def variable[A](attributes: A, name: Name): Value[Nothing, A]   = Variable(attributes, name)
  final def variable[A](attributes: A, name: String): Value[Nothing, A] = Variable(attributes, name)
  final def variable(name: Name): RawValue                              = Variable.Raw(name)
  final def variable(name: String): RawValue                            = Variable.Raw(name)
}
