package zio.morphir.ir.value.recursive

import zio.morphir.ir.Name

trait ValueConstructors {
  import Value._

  def variable[A](attributes: A, name: Name): Value[Nothing, A]   = Variable(attributes, name)
  def variable[A](attributes: A, name: String): Value[Nothing, A] = Variable(attributes, name)
  def variable(name: Name): RawValue                              = Variable.Raw(name)
  def variable(name: String): RawValue                            = Variable.Raw(name)
}
