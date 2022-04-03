package zio.morphir.ir.types

import zio.Chunk
import zio.morphir.ir._

final case class TypeExpr[+A](caseValue: TypeCase[A, TypeExpr[A]]) {
  import TypeCase._
  def fold[Z](f: TypeCase[A, Z] => Z): Z = caseValue match {
    case c @ ExtensibleRecordCase(_, _, _) =>
      f(ExtensibleRecordCase(c.attributes, c.name, c.fields.map(_.map(_.fold(f)))))
    case c @ FunctionCase(_, _, _)  => f(FunctionCase(c.attributes, c.paramTypes.map(_.fold(f)), c.returnType.fold(f)))
    case c @ RecordCase(_, _)       => f(RecordCase(c.attributes, c.fields.map(_.map(_.fold(f)))))
    case c @ ReferenceCase(_, _, _) => f(ReferenceCase(c.attributes, c.typeName, c.typeParams.map(_.fold(f))))
    case c @ TupleCase(_, _)        => f(TupleCase(c.attributes, c.elements.map(_.fold(f))))
    case c @ UnitCase(_)            => f(c)
    case c @ VariableCase(_, _)     => f(c)
  }
}

object TypeExpr {
  import TypeCase._

  def extensibleRecord[A](attributes: A, name: Name, fields: Chunk[Field[TypeExpr[A]]]) =
    TypeExpr(ExtensibleRecordCase(attributes, name, fields))

  def function[A](attributes: A, paramTypes: Chunk[TypeExpr[A]], returnType: TypeExpr[A]) =
    TypeExpr(FunctionCase(attributes, paramTypes, returnType))

  def record[A](attributes: A, fields: Chunk[Field[TypeExpr[A]]]) =
    TypeExpr(RecordCase(attributes, fields))

  def reference[A](attributes: A, typeName: FQName, typeParams: Chunk[TypeExpr[A]]) =
    TypeExpr(ReferenceCase(attributes, typeName, typeParams))

  def tuple[A](attributes: A, elements: Chunk[TypeExpr[A]]) =
    TypeExpr(TupleCase(attributes, elements))

  def unit[A](attributes: A) = TypeExpr(UnitCase(attributes))

  def variable[A](attributes: A, name: Name): TypeExpr[A] =
    TypeExpr(VariableCase(attributes, name))

  // TODO: When we switch back to Recursion schemes being the main encoding change this to TypeExpr and TypeExpr to Type
  type Type = TypeExpr[Any]
  object Type {}
}
