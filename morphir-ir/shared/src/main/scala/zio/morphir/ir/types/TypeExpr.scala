package zio.morphir.ir.types

import zio.Chunk
import zio.morphir.ir._
import zio.prelude._

final case class TypeExpr[+A](caseValue: TypeCase[A, TypeExpr[A]]) { self =>
  import TypeCase._

  def ??(doc: String): Documented[TypeExpr[A]] = Documented(doc, self)

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

  def foldDown[Z](z: Z)(f: (Z, TypeExpr[A]) => Z): Z =
    caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, TypeExpr[A]), Z]): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: TypeCase[A, Z] => F[Z]): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldUp[Z](z: Z)(f: (Z, TypeExpr[A]) => Z): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)
}

object TypeExpr {
  import TypeCase._

  def extensibleRecord[A](attributes: A, name: Name, fields: Chunk[Field[TypeExpr[A]]]): TypeExpr[A] =
    TypeExpr(ExtensibleRecordCase(attributes, name, fields))

  def function[A](attributes: A, paramTypes: Chunk[TypeExpr[A]], returnType: TypeExpr[A]): TypeExpr[A] =
    TypeExpr(FunctionCase(attributes, paramTypes, returnType))

  def record[A](attributes: A, fields: Chunk[Field[TypeExpr[A]]]): TypeExpr[A] =
    TypeExpr(RecordCase(attributes, fields))

  def reference[A](attributes: A, typeName: FQName, typeParams: Chunk[TypeExpr[A]]): TypeExpr[A] =
    TypeExpr(ReferenceCase(attributes, typeName, typeParams))

  def tuple[A](attributes: A, elements: Chunk[TypeExpr[A]]): TypeExpr[A] =
    TypeExpr(TupleCase(attributes, elements))

  def unit[A](attributes: A): TypeExpr[A] = TypeExpr(UnitCase(attributes))

  def variable[A](attributes: A, name: Name): TypeExpr[A] =
    TypeExpr(VariableCase(attributes, name))

  // TODO: When we switch back to Recursion schemes being the main encoding change this to TypeExpr and TypeExpr to Type
  type Type = TypeExpr[Any]
  object Type {}
}
