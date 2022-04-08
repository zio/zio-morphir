package zio.morphir.ir.types.recursive

import zio.morphir.ir._
import zio.prelude._
import zio.prelude.fx.ZPure

import zio.{Chunk, ZIO}

final case class Type[+A](caseValue: TypeCase[A, Type[A]]) { self =>
  import TypeCase._
  import Type._

  def ??(doc: String): Documented[Type[A]] = Documented(doc, self)

  def attributes: A = caseValue.attributes

  def collectReferences: Set[FQName] = fold[Set[FQName]] {
    case TypeCase.ExtensibleRecordCase(_, _, fields)      => fields.map(_.data).flatten.toSet
    case TypeCase.FunctionCase(_, paramTypes, returnType) => paramTypes.flatten.toSet ++ returnType
    case TypeCase.RecordCase(_, fields)                   => fields.map(_.data).flatten.toSet
    case TypeCase.ReferenceCase(_, name, typeParams)      => typeParams.flatten.toSet + name
    case TypeCase.TupleCase(_, elementTypes)              => elementTypes.flatten.toSet
    case TypeCase.UnitCase(_)                             => Set.empty
    case TypeCase.VariableCase(_, _)                      => Set.empty
  }

  def collectVariables: Set[Name] = fold[Set[Name]] {
    case TypeCase.ExtensibleRecordCase(_, name, fields)   => fields.map(_.data).flatten.toSet + name
    case TypeCase.FunctionCase(_, paramTypes, returnType) => paramTypes.flatten.toSet ++ returnType
    case TypeCase.RecordCase(_, fields)                   => fields.map(_.data).flatten.toSet
    case TypeCase.ReferenceCase(_, _, typeParams)         => typeParams.flatten.toSet
    case TypeCase.TupleCase(_, elementTypes)              => elementTypes.flatten.toSet
    case TypeCase.UnitCase(_)                             => Set.empty
    case TypeCase.VariableCase(_, name)                   => Set(name)
  }

  /**
   * Erase the attributes from this type.
   */
  def eraseAttributes: UType = self.mapAttributes(_ => ())

  def fields: Chunk[Field[Type[A]]] = fold[Chunk[Field[Type[A]]]] {
    case ExtensibleRecordCase(_, _, fields) => fields.map(_.data).flatten
    case RecordCase(_, fields)              => fields.map(_.data).flatten
    case _                                  => Chunk.empty
  }

  def fieldCount: Int = fold[Int] {
    case ExtensibleRecordCase(_, _, fields) => fields.map(_.data).sum + fields.size
    case RecordCase(_, fields)              => fields.map(_.data).sum + fields.size
    case _                                  => 0
  }

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

  def foldDown[Z](z: Z)(f: (Z, Type[A]) => Z): Z =
    caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Type[A]), Z]): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: TypeCase[A, Z] => F[Z]): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldPure[W, S, R, E, Z](f: TypeCase[A, Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
    foldM(f)

  def foldRecursive[Z](f: TypeCase[A, (Type[A], Z)] => Z): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

  def foldUp[Z](z: Z)(f: (Z, Type[A]) => Z): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

  def foldZIO[R, E, Z](f: TypeCase[A, Z] => ZIO[R, E, Z]): ZIO[R, E, Z] = foldM(f)

  def map[B](f: A => B): Type[B] =
    self.fold[Type[B]] {
      case ExtensibleRecordCase(attributes, name, fields) => Type(ExtensibleRecordCase(f(attributes), name, fields))
      case FunctionCase(attributes, paramTypes, returnType) =>
        Type(FunctionCase(f(attributes), paramTypes, returnType))
      case RecordCase(attributes, fields) => Type(RecordCase(f(attributes), fields))
      case ReferenceCase(attributes, typeName, typeParams) =>
        Type(ReferenceCase(f(attributes), typeName, typeParams))
      case TupleCase(attributes, elements) => Type(TupleCase(f(attributes), elements))
      case UnitCase(attributes)            => Type(UnitCase(f(attributes)))
      case VariableCase(attributes, name)  => Type(VariableCase(f(attributes), name))
    }

  @inline def mapAttributes[B](f: A => B): Type[B] = map(f)

  def satisfiesCaseOf(check: PartialFunction[TypeCase[A, Type[A]], Boolean]): Boolean =
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

object Type extends TypeExprConstructors with UnattributedTypeExprConstructors with FieldSyntax {
  import TypeCase._
  type FieldT[A] = Field[Type[A]]

  type UType = Type[Any]
  val UType = Type

  def mapTypeAttributes[A](tpe: Type[A]): MapTypeAttributes[A] = new MapTypeAttributes(() => tpe)

  object ExtensibleRecord {
    def apply[A](attributes: A, name: Name, fields: Chunk[FieldT[A]])(implicit ev: NeedsAttributes[A]): Type[A] =
      Type(ExtensibleRecordCase(attributes, name, fields))

    def apply[A](attributes: A, name: Name, fields: FieldT[A]*)(implicit ev: NeedsAttributes[A]): Type[A] =
      Type(ExtensibleRecordCase(attributes, name, Chunk.fromIterable(fields)))

    def apply[A](attributes: A, name: String, fields: FieldT[A]*)(implicit ev: NeedsAttributes[A]): Type[A] =
      Type(ExtensibleRecordCase(attributes, Name.fromString(name), Chunk.fromIterable(fields)))

    def unapply[A](self: Type[A]): Option[(A, Name, Chunk[FieldT[A]])] =
      self.caseValue match {
        case ExtensibleRecordCase(attributes, name, fields) => Some((attributes, name, fields))
        case _                                              => None
      }
  }

  object Function {
    def apply[A](attributes: A, paramTypes: Chunk[Type[A]], returnType: Type[A])(implicit
        ev: NeedsAttributes[A]
    ): Type[A] =
      Type(FunctionCase(attributes, paramTypes, returnType))

    def apply[A](attributes: A, paramTypes: Type[A]*)(returnType: Type[A])(implicit
        ev: NeedsAttributes[A]
    ): Type[A] =
      Type(FunctionCase(attributes, Chunk.fromIterable(paramTypes), returnType))

    def unapply[A](self: Type[A]): Option[(A, Chunk[Type[A]], Type[A])] =
      self.caseValue match {
        case FunctionCase(attributes, paramTypes, returnType) => Some((attributes, paramTypes, returnType))
        case _                                                => None
      }
  }

  object Record {
    def apply[A](attributes: A, fields: Chunk[FieldT[A]])(implicit ev: NeedsAttributes[A]): Type[A] =
      Type(RecordCase(attributes, fields))

    def apply[A](attributes: A, fields: FieldT[A]*)(implicit ev: NeedsAttributes[A]): Type[A] =
      Type(RecordCase(attributes, Chunk.fromIterable(fields)))

    def unapply[A](self: Type[A]): Option[(A, Chunk[FieldT[A]])] =
      self.caseValue match {
        case RecordCase(attributes, fields) => Some((attributes, fields))
        case _                              => None
      }
  }

  object Reference {
    def apply[A](attributes: A, name: FQName, typeParams: Chunk[Type[A]])(implicit
        ev: NeedsAttributes[A]
    ): Type[A] =
      Type(ReferenceCase(attributes, name, typeParams))

    def apply[A](attributes: A, name: FQName, typeParams: Type[A]*)(implicit ev: NeedsAttributes[A]): Type[A] =
      Type(ReferenceCase(attributes, name, Chunk.fromIterable(typeParams)))

    def unapply[A](self: Type[A]): Option[(A, FQName, Chunk[Type[A]])] =
      self.caseValue match {
        case ReferenceCase(attributes, name, typeParams) => Some((attributes, name, typeParams))
        case _                                           => None
      }
  }

  object Tuple {
    def apply[A](attributes: A, elements: Chunk[Type[A]])(implicit ev: NeedsAttributes[A]): Type[A] =
      tuple(attributes, elements)

    def apply[A](attributes: A, elements: Type[A]*)(implicit ev: NeedsAttributes[A]): Type[A] =
      tuple(attributes, elements: _*)

    def unapply[A](t: Type[A]): Option[(A, Chunk[Type[A]])] =
      t.caseValue match {
        case TupleCase(attributes, elements) => Some(attributes -> elements)
        case _                               => None
      }
  }

  object Unit {
    def apply[A](attributes: A): Type[A] = unit(attributes)
    def unapply[A](self: Type[A]): Option[A] = self.caseValue match {
      case UnitCase(attributes) => Some(attributes)
      case _                    => None
    }
  }

  object Variable {
    def apply[A](attributes: A, name: String): Type[A] = variable(attributes, name)
    def apply[A](attributes: A, name: Name): Type[A]   = variable(attributes, name)
    def unapply[A](self: Type[A]): Option[(A, Name)] = self.caseValue match {
      case VariableCase(attributes, name) => Some(attributes -> name)
      case _                              => None
    }
  }

  final class MapTypeAttributes[+A](val input: () => Type[A]) extends AnyVal {
    def apply[B](f: A => B): Type[B] = input().map(f)
  }
}
