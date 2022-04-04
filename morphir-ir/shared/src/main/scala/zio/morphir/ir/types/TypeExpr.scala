package zio.morphir.ir.types

import zio.{Chunk, ZIO}
import zio.morphir.ir._
import zio.prelude._
import zio.prelude.fx.ZPure

final case class TypeExpr[+A](caseValue: TypeCase[A, TypeExpr[A]]) { self =>
  import TypeCase._
  import TypeExpr._

  def ??(doc: String): Documented[TypeExpr[A]] = Documented(doc, self)

  def collectReferences: Set[FQName] = fold[Set[FQName]] {
    case TypeCase.ExtensibleRecordCase(_, _, fields)      => fields.map(_.fieldType).flatten.toSet
    case TypeCase.FunctionCase(_, paramTypes, returnType) => paramTypes.flatten.toSet ++ returnType
    case TypeCase.RecordCase(_, fields)                   => fields.map(_.fieldType).flatten.toSet
    case TypeCase.ReferenceCase(_, name, typeParams)      => typeParams.flatten.toSet + name
    case TypeCase.TupleCase(_, elementTypes)              => elementTypes.flatten.toSet
    case TypeCase.UnitCase(_)                             => Set.empty
    case TypeCase.VariableCase(_, _)                      => Set.empty
  }

  def collectVariables: Set[Name] = fold[Set[Name]] {
    case TypeCase.ExtensibleRecordCase(_, name, fields)   => fields.map(_.fieldType).flatten.toSet + name
    case TypeCase.FunctionCase(_, paramTypes, returnType) => paramTypes.flatten.toSet ++ returnType
    case TypeCase.RecordCase(_, fields)                   => fields.map(_.fieldType).flatten.toSet
    case TypeCase.ReferenceCase(_, _, typeParams)         => typeParams.flatten.toSet
    case TypeCase.TupleCase(_, elementTypes)              => elementTypes.flatten.toSet
    case TypeCase.UnitCase(_)                             => Set.empty
    case TypeCase.VariableCase(_, name)                   => Set(name)
  }

  /**
   * Erase the attributes from this type.
   */
  def eraseAttributes: Type = self.mapAttributes(_ => ())

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

  def foldPure[W, S, R, E, Z](f: TypeCase[A, Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
    foldM(f)

  def foldRecursive[Z](f: TypeCase[A, (TypeExpr[A], Z)] => Z): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

  def foldUp[Z](z: Z)(f: (Z, TypeExpr[A]) => Z): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

  def foldZIO[R, E, Z](f: TypeCase[A, Z] => ZIO[R, E, Z]): ZIO[R, E, Z] = foldM(f)

  def map[B](f: A => B): TypeExpr[B] =
    self.fold[TypeExpr[B]] {
      case ExtensibleRecordCase(attributes, name, fields) => TypeExpr(ExtensibleRecordCase(f(attributes), name, fields))
      case FunctionCase(attributes, paramTypes, returnType) =>
        TypeExpr(FunctionCase(f(attributes), paramTypes, returnType))
      case RecordCase(attributes, fields) => TypeExpr(RecordCase(f(attributes), fields))
      case ReferenceCase(attributes, typeName, typeParams) =>
        TypeExpr(ReferenceCase(f(attributes), typeName, typeParams))
      case TupleCase(attributes, elements) => TypeExpr(TupleCase(f(attributes), elements))
      case UnitCase(attributes)            => TypeExpr(UnitCase(f(attributes)))
      case VariableCase(attributes, name)  => TypeExpr(VariableCase(f(attributes), name))
    }

  @inline def mapAttributes[B](f: A => B): TypeExpr[B] = map(f)

  def satisfiesCaseOf(check: PartialFunction[TypeCase[A, TypeExpr[A]], Boolean]): Boolean =
    check.lift(self.caseValue).getOrElse(false)

  override def toString: String = fold[String] {
    case ExtensibleRecordCase(_, name, fields) => s"{ ${name.toCamelCase} | ${fields.mkString(", ")} }"
    case FunctionCase(_, paramTypes, returnType) =>
      paramTypes
        .map(_.toString)
        .mkString("(", ",", ")")
        .concat(" -> " + returnType.toString)
    case RecordCase(_, fields)              => fields.mkString("{ ", ", ", " }")
    case ReferenceCase(_, name, typeParams) => s"${name.toString} ${typeParams.mkString(" ")}"
    case TupleCase(_, elementTypes)         => elementTypes.mkString("(", ", ", ")")
    case UnitCase(_)                        => "()"
    case VariableCase(_, name)              => name.toCamelCase
  }
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
