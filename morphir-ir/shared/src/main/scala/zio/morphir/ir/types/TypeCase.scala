package zio.morphir.ir.types
import zio.Chunk
import zio.morphir.ir._
import zio.morphir.ir.types.TypeCase._

sealed trait TypeCase[+A, +Self] { self =>
  def attributes: A

  final def map[Self2](f: Self => Self2): TypeCase[A, Self2] =
    self match {
      case c @ ExtensibleRecordCase(_, _, _) => ExtensibleRecordCase(c.attributes, c.name, c.fields.map(_.map(f)))
      case c @ FunctionCase(_, _, _)         => FunctionCase(c.attributes, c.paramTypes.map(f), f(c.returnType))
      case c @ RecordCase(_, _)              => RecordCase(c.attributes, c.fields.map(_.map(f)))
      case c @ ReferenceCase(_, _, _)        => ReferenceCase(c.attributes, c.typeName, c.typeParams.map(f))
      case c @ TupleCase(_, _)               => TupleCase(c.attributes, c.elements.map(f))
      case c @ UnitCase(_)                   => c
      case c @ VariableCase(_, _)            => c
    }

}

object TypeCase {
  final case class ExtensibleRecordCase[+A, +Self](attributes: A, name: Name, fields: Chunk[Field[Self]])
      extends TypeCase[A, Self]
  final case class FunctionCase[+A, +Self](attributes: A, paramTypes: Chunk[Self], returnType: Self)
      extends TypeCase[A, Self]
  final case class RecordCase[+A, +Self](attributes: A, fields: Chunk[Field[Self]]) extends TypeCase[A, Self]
  final case class ReferenceCase[+A, +Self](attributes: A, typeName: FQName, typeParams: Chunk[Self])
      extends TypeCase[A, Self]
  final case class TupleCase[+A, +Self](attributes: A, elements: Chunk[Self]) extends TypeCase[A, Self]
  final case class UnitCase[+A](attributes: A)                                extends TypeCase[A, Nothing]
  final case class VariableCase[+A](attributes: A, name: Name)                extends TypeCase[A, Nothing]
}



// trait TypeMaker[A, Self] {
//   def make(caseValue: TypeCase[A, Self]): TypeInstance
// }
