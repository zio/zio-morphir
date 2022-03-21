package zio.morphir.ir

import zio.{Chunk, ZIO}
import zio.prelude._
import zio.morphir.syntax.TypeModuleSyntax
import zio.prelude.fx.ZPure

import scala.annotation.tailrec
object TypeModule extends TypeModuleSyntax {

  final case class Field[+T](name: Name, fieldType: T) { self =>

    /**
     * An alias for `attributeTypeWith`.
     */
    def @@[Attributes0, Attributes](f: Attributes0 => Attributes)(implicit
        ev: T <:< Type[Attributes0]
    ): Field[Type[Attributes]] =
      attributeTypeWith(f)

    /**
     * Attributes the field with the given `attributes`.
     */
    def attributeTypeAs[Attributes](attributes: => Attributes)(implicit ev: T <:< Type[_]): Field[Type[Attributes]] =
      Field(name, fieldType.mapAttributes(_ => attributes))

    /**
     * Attributes the field's type using the given function.
     */
    def attributeTypeWith[Attributes0, Attributes](f: Attributes0 => Attributes)(implicit
        ev: T <:< Type[Attributes0]
    ): Field[Type[Attributes]] =
      Field(name, fieldType.mapAttributes(f))

    def forEach[G[+_]: IdentityBoth: Covariant, U](f: T => G[U]): G[Field[U]] =
      f(self.fieldType).map(newType => self.copy(fieldType = newType))

    def map[U](f: T => U): Field[U] = Field(name, f(fieldType))

    def mapAttributes[Attributes0, Attributes1](f: Attributes0 => Attributes1)(implicit
        ev: T <:< Type[Attributes0]
    ): Field[Type[Attributes1]] =
      Field(name, fieldType.mapAttributes(f))

  }

  final case class Constructors[+Attributes](toMap: Map[Name, Chunk[(Name, Type[Attributes])]]) extends AnyVal { self =>
    def eraseAttributes: Constructors[Any] = Constructors(toMap.map { case (ctor, args) =>
      (ctor, args.map { case (paramName, paramType) => (paramName, paramType.eraseAttributes) })
    })

    def collectReferences: Set[FQName] = {
      toMap.values.flatMap {
        case Chunk((_, tpe)) =>
          tpe.collectReferences
        case _ => Nil
      }.toSet
    }

    def ctorNames: Set[Name] = toMap.keySet
  }

  object Constructors {

    def forEnum(case1: String, otherCases: String*): Constructors[Any] = {
      val allCases  = (Chunk(case1) ++ otherCases).map(Name.fromString)
      val emptyArgs = Chunk[(Name, UType)]()
      Constructors(allCases.map(name => (name, emptyArgs)).toMap)
    }

  }

  sealed trait Definition[+Attributes] { self =>
    import Definition._
    import Specification._

    def toSpecification: Specification[Attributes] = self match {
      case TypeAlias(typeParams, typeExp) =>
        TypeAliasSpecification[Attributes](typeParams, typeExp)
      case CustomType(typeParams: Chunk[Name], ctors) if ctors.withPublicAccess.isDefined =>
        val constructors: Constructors[Attributes] = ctors.withPublicAccess.get
        // val annotations = constructors.items.values.map(_.tpe.annotations).reduce(_ ++ _) // ???
        CustomTypeSpecification[Attributes](typeParams, constructors)
      case CustomType(typeParams, _) =>
        OpaqueTypeSpecification(typeParams) // TODO fix annotations
    }

    // def eraseAttributes: Definition[Nothing] = self match {
    //   case TypeAlias(typeParams, typeExp) =>
    //     TypeAlias(typeParams, typeExp.eraseAttributes)
    //   case CustomType(typeParams, ctors) =>
    //     CustomType(typeParams, ctors.eraseAttributes)
    // }
  }

  object Definition {
    final case class TypeAlias[+Attributes](typeParams: Chunk[Name], typeExp: Type[Attributes])
        extends Definition[Attributes]

    final case class CustomType[+Attributes](
        typeParams: Chunk[Name],
        ctors: AccessControlled[Constructors[Attributes]]
    ) extends Definition[Attributes]
  }

  type USpecification = Specification[Any]
  val USpecification = Specification
  sealed trait Specification[+Attributes] { self =>
    import Specification._

    def ??(doc: String): Documented[Specification[Attributes]] =
      Documented(doc, self)

    // def map[Annotations0 >: Annotations](f: Annotations => Annotations0): Specification[Annotations0] = self match {
    //   case c @ TypeAliasSpecification(_, _, _) =>
    //     TypeAliasSpecification[Annotations0](c.typeParams, c.expr.map(f), c.annotations.map(f))
    //   case c @ OpaqueTypeSpecification(_, _) =>
    //     OpaqueTypeSpecification[Annotations0](c.typeParams, c.annotations.map(f))
    //   case c @ CustomTypeSpecification(_, _, _) =>
    //     CustomTypeSpecification[Annotations0](c.typeParams, c.ctors.map(f), c.annotations.map(f))
    // }
    def eraseAttributes: Specification[Any] = self match {
      case c @ TypeAliasSpecification(_, _) =>
        TypeAliasSpecification(c.typeParams, c.expr.eraseAttributes)
      case c @ OpaqueTypeSpecification(_) =>
        OpaqueTypeSpecification(c.typeParams)
      case c @ CustomTypeSpecification(_, _) =>
        CustomTypeSpecification(c.typeParams, c.ctors.eraseAttributes)
    }
  }

  object Specification {
    final case class TypeAliasSpecification[+Attributes](
        typeParams: Chunk[Name],
        expr: Type[Attributes]
    ) extends Specification[Attributes]

    final case class OpaqueTypeSpecification(typeParams: Chunk[Name]) extends Specification[Nothing]

    object OpaqueTypeSpecification {
      def apply(typeParams: String*): OpaqueTypeSpecification =
        OpaqueTypeSpecification(Chunk.fromIterable(typeParams.map(Name.fromString)))
    }

    final case class CustomTypeSpecification[+Attributes](
        typeParams: Chunk[Name],
        ctors: Constructors[Attributes]
    ) extends Specification[Attributes]

    object CustomTypeSpecification {
      def fromCtors[Attributes](
          ctor: (String, Iterable[(String, Type[Attributes])]),
          ctors: (String, Iterable[(String, Type[Attributes])])*
      ): CustomTypeSpecification[Attributes] = {
        val allCtors = (ctor +: ctors).map { case (name, args) =>
          (
            Name.fromString(name),
            Chunk.fromIterable(args.map { case (name, tpe) =>
              (Name.fromString(name), tpe)
            })
          )
        }.toMap
        CustomTypeSpecification(Chunk.empty, Constructors(allCtors))
      }

      def mkEnum(case1: String, otherCases: String*): CustomTypeSpecification[Any] =
        CustomTypeSpecification(Chunk.empty, Constructors.forEnum(case1, otherCases: _*))
    }

    type UCustomTypeSpecification = CustomTypeSpecification[Any]
    val UCustomTypeSpecification: CustomTypeSpecification.type = CustomTypeSpecification
  }

  sealed trait Type[+Attributes] { self =>
    def @@[Attributes2](f: Attributes => Attributes2): Type[Attributes2] =
      mapAttributes(f)

    def ??(doc: String): Documented[Type[Attributes]] = Documented(doc, self)

    def attributes: Attributes

    def collect[Z](pf: PartialFunction[Type[Attributes], Z]): Chunk[Z] =
      foldLeft[Chunk[Z]](Chunk.empty) { case (acc, tpe) =>
        if (pf.isDefinedAt(tpe)) acc :+ pf(tpe)
        else acc
      }

    def foldLeft[Z](zero: Z)(f: (Z, Type[Attributes]) => Z): Z = {
      @tailrec
      def loop(types: List[Type[Attributes]], acc: Z): Z = types match {
        case (tpe @ Type.ExtensibleRecord(_, _, _)) :: tail =>
          loop(tpe.fields.map(_.fieldType).toList ++ tail, f(acc, tpe))
        case (tpe @ Type.Function(_, _, _)) :: tail =>
          loop(tpe.paramTypes.toList ++ (tpe.returnType :: tail), f(acc, tpe))
        case (tpe @ Type.Record(_, _)) :: tail =>
          loop(tpe.fields.map(_.fieldType).toList ++ tail, f(acc, tpe))
        case (tpe @ Type.Reference(_, _, _)) :: tail =>
          loop(tpe.typeParams.toList ++ tail, f(acc, tpe))
        case (tpe @ Type.Tuple(_, elements)) :: tail =>
          loop(elements.toList ++ tail, f(acc, tpe))
        case Type.Variable(_, _) :: tail => loop(tail, acc)
        case Type.Unit(_) :: tail        => loop(tail, acc)
        case Nil                         => acc
      }
      loop(List(self), zero)
    }

//    def foldDown[Z](z: Z)(f: (Z, Type[Attributes]) => Z): Z =
//      caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))
//
//    def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Type[Attributes]), Z]): Z =
//      foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))
//
//    def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: TypeCase[Z] => F[Z]): F[Z] =
//      fold[F[Z]](_.flip.flatMap(f))
//
//    def foldPure[W, S, R, E, Z](f: TypeCase[Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
//      foldM(f)
//
//    def transformDown[Annotations0 >: Attributes](
//        f: Type[Annotations0] => Type[Annotations0]
//    ): Type[Annotations0] = {
//      def loop(recursive: Type[Annotations0]): Type[Attributes] =
//        Type(f(recursive).caseValue.map(loop), attributes)
//      loop(self)
//    }
//
//    def foldZIO[R, E, Z](f: TypeCase[Z] => ZIO[R, E, Z]): ZIO[R, E, Z] =
//      foldM(f)
//
//    def foldRecursive[Z](f: TypeCase[(Type[Attributes], Z)] => Z): Z =
//      f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))
//
//    def foldUp[Z](z: Z)(f: (Z, Type[Attributes]) => Z): Z =
//      f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)
//
//    def foldUpSome[Z](z: Z)(pf: PartialFunction[(Z, Type[Attributes]), Z]): Z =
//      foldUp(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

    // TODO: See if we can refactor to be stack safe/ tail recursive
    def mapAttributes[Attributes2](f: Attributes => Attributes2): Type[Attributes2] = self match {
      case Type.ExtensibleRecord(attributes, name, fields) =>
        Type.ExtensibleRecord(f(attributes), name, fields.map(_.mapAttributes(f)))
      case Type.Function(attributes, paramTypes, returnType) =>
        Type.Function(f(attributes), paramTypes.map(_.mapAttributes(f)), returnType.mapAttributes(f))
      case Type.Record(attributes, fields) => Type.Record(f(attributes), fields.map(_.mapAttributes(f)))
      case Type.Reference(attributes, typeName, typeParams) =>
        Type.Reference(f(attributes), typeName, typeParams.map(_.mapAttributes(f)))
      case Type.Tuple(attributes, elementTypes) => Type.Tuple(f(attributes), elementTypes.map(_.mapAttributes(f)))
      case Type.Unit(attributes)                => Type.Unit(f(attributes))
      case Type.Variable(attributes, name)      => Type.Variable(f(attributes), name)
    }

//    def collectVariables: Set[Name] = fold[Set[Name]] {
//      case c @ TypeCase.ExtensibleRecordCase(_, _)       => c.fields.map(_.fieldType).flatten.toSet + c.name
//      case TypeCase.FunctionCase(paramTypes, returnType) => paramTypes.flatten.toSet ++ returnType
//      case TypeCase.RecordCase(fields)                   => fields.map(_.fieldType).flatten.toSet
//      case TypeCase.ReferenceCase(_, typeParams)         => typeParams.flatten.toSet
//      case TypeCase.TupleCase(elementTypes)              => elementTypes.flatten.toSet
//      case TypeCase.UnitCase                             => Set.empty
//      case TypeCase.VariableCase(name)                   => Set(name)
//    }
//

    def collectReferences: Set[FQName] = foldLeft(Set.empty[FQName]) { case (acc, tpe) =>
      tpe match {
        case Type.Reference(_, typeName, _) => acc + typeName
        case _                              => acc
      }
    }

    def collectVariables: Set[Name] = foldLeft(Set.empty[Name]) { case (acc, tpe) =>
      tpe match {
        case tpe @ Type.ExtensibleRecord(_, _, _) => acc + tpe.name
        case Type.Variable(_, name)               => acc + name
        case _                                    => acc
      }
    }
//    def collectReferences: Set[FQName] = foldLeft(Set.empty[FQName]) {
//      case (acc, c @ Type.ExtensibleRecord(_, _)) => ??? // c.fields.map(_.fieldType).flatten.toSet
//      case TypeCase.FunctionCase(paramTypes, returnType) =>
//        paramTypes.flatten.toSet ++ returnType
//      case TypeCase.RecordCase(fields) =>
//        fields.map(_.fieldType).flatten.toSet
//      case TypeCase.ReferenceCase(name, typeParams) =>
//        typeParams.flatten.toSet + name
//      case TypeCase.TupleCase(elementTypes) => elementTypes.flatten.toSet
//      case TypeCase.UnitCase                => Set.empty
//      case TypeCase.VariableCase(_)         => Set.empty
//    }

    /**
     * Erase the attributes from this type.
     */
    def eraseAttributes: UType = self.mapAttributes(_ => Type.emptyAttributes)

    // TO DO
    // def substituteTypeVariables(mapping: Map[Name, Type[Annotations]]): Type[Annotations] = self.caseValue match {
    //   case TypeCase.VariableCase(name) =>
    //     mapping.getOrElse(name, self)
    //   case TypeCase.ExtensibleRecordCase(name, fields) =>
    //     TypeCase.ExtensibleRecordCase(name, fields.map(_.substituteTypeVariables(mapping)))
    //   case TypeCase.FieldCase(name, fieldType) =>
    //     TypeCase.FieldCase(name, fieldType.substituteTypeVariables(mapping))
    //   case TypeCase.FunctionCase(paramTypes, returnType) =>
    //     TypeCase.FunctionCase(paramTypes.map(_.substituteTypeVariables(mapping)), returnType.substituteTypeVariables(mapping))
    //   case TypeCase.RecordCase(fields) =>
    //     TypeCase.RecordCase(fields.map(_.substituteTypeVariables(mapping)))
    //   case TypeCase.ReferenceCase(fqName, typeParams) =>
    //     TypeCase.ReferenceCase(fqName, typeParams.map(_.substituteTypeVariables(mapping)))
    //   case TypeCase.TupleCase(elementTypes) =>
    //     TypeCase.TupleCase(elementTypes.map(_.substituteTypeVariables(mapping)))
    //   case TypeCase.UnitCase =>
    //     TypeCase.UnitCase
    // }

    def satisfiesCaseOf(check: PartialFunction[Type[Attributes], Boolean]): Boolean =
      check.lift(self).getOrElse(false)
//
//    override def toString: String = fold[String] {
//      case c @ Type.ExtensibleRecord(_, _) =>
//        s"{ ${c.name.toCamel} | ${c.fields.mkString(", ")} }"
//       Type.Function(paramTypes, returnType) =>
//        paramTypes
//          .map(_.toString)
//          .mkString("(", ",", ")")
//          .concat(" -> " + returnType.toString)
//       Type.Record(fields)              => fields.mkString("{ ", ", ", " }")
//       Type.Reference(name, typeParams) => s"${name.toString} ${typeParams.mkString(" ")}"
//       Type.Tuple(elementTypes)         => elementTypes.mkString("(", ", ", ")")
//       Type.Unit                        => "()"
//       Type.Variable(name)              => name.toCamel
//    }
  }

  object Type extends TypeModuleSyntax {

    lazy val emptyAttributes: scala.Unit = ()

    final case class ExtensibleRecord[+Attributes](
        attributes: Attributes,
        name: Name,
        fields: Chunk[Field[Type[Attributes]]]
    ) extends Type[Attributes]
    final case class Function[+Attributes](
        attributes: Attributes,
        paramTypes: Chunk[Type[Attributes]],
        returnType: Type[Attributes]
    ) extends Type[Attributes]
    final case class Record[+Attributes](attributes: Attributes, fields: Chunk[Field[Type[Attributes]]])
        extends Type[Attributes]
    final case class Reference[+Attributes](
        attributes: Attributes,
        typeName: FQName,
        typeParams: Chunk[Type[Attributes]]
    ) extends Type[Attributes]
    final case class Tuple[+Attributes](attributes: Attributes, elementTypes: Chunk[Type[Attributes]])
        extends Type[Attributes]
    final case class Unit[+Attributes](attributes: Attributes)                 extends Type[Attributes]
    final case class Variable[+Attributes](attributes: Attributes, name: Name) extends Type[Attributes]

  }

  type UConstructors = Constructors[Any]
  val UConstructors: Constructors.type = Constructors

  /** Represents an un-annotated type. */
  type UType = Type[Unit]
  val UType = Type
}
