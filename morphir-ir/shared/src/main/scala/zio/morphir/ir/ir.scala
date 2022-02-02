package zio.morphir.ir

import zio.morphir.ir.recursive.*
import zio.Chunk

sealed trait TypeTree extends IR { self =>
  import TypeTreeCase.*
  import DefinitionCase.*
  import SpecificationCase.*
  import TypeCase.*

  override def caseValue: TypeTreeCase[TypeTree]

  // def fold[Z](f: TypeTreeCase[Z] => Z): Z = self.caseValue match {
  //   case c @ ConstructorsCase(_) => f(ConstructorsCase(c.args.map { case (name, tree) => (name, tree.fold(f)) }))
  //   case c @ CustomTypeDefinitionCase(_, _)    => f(CustomTypeDefinitionCase(c.typeParams, c.ctors.map(_.fold(f))))
  //   case c @ TypeAliasDefinitionCase(_, _)     => f(TypeAliasDefinitionCase(c.typeParams, c.typeExpr.fold(f)))
  //   case c @ CustomTypeSpecificationCase(_, _) => f(CustomTypeSpecificationCase(c.typeParams, c.ctors.fold(f)))
  //   case c @ OpaqueTypeSpecificationCase(_)    => f(c)
  //   case c @ TypeAliasSpecificationCase(_, _)  => f(TypeAliasSpecificationCase(c.typeParams, c.typeExpr.fold(f)))
  //   case c @ ExtensibleRecordCase(_, _)        => f(ExtensibleRecordCase(c.name, c.fields.map(_.fold(f))))
  //   case c @ FunctionCase(_, _)                => f(FunctionCase(c.paramTypes.map(_.fold(f)), c.returnType.fold(f)))
  //   case c @ RecordCase(_)                     => f(RecordCase(c.fields.map(_.fold(f))))
  //   case c @ ReferenceCase(_, _)               => f(ReferenceCase(c.typeName, c.typeParams.map(_.fold(f))))
  //   case c @ TupleCase(_)                      => f(TupleCase(c.elementTypes.map(_.fold(f))))
  //   case UnitCase                              => f(UnitCase)
  //   case c @ VariableCase(_)                   => f(c)
  //   case c @ FieldCase(_, _)                   => f(FieldCase(c.name, c.fieldType.fold(f)))
  // }
}

object TypeTree {
  import TypeTreeCase.*
  import DefinitionCase.*
  import SpecificationCase.*
  final case class Constructors(args: Map[Name, Type]) extends TypeTree {
    override lazy val caseValue: TypeTreeCase[TypeTree] = ConstructorsCase(args)
  }

  sealed trait Definition extends TypeTree { self =>
    def typeParams: List[Name]
    override def caseValue: DefinitionCase[TypeTree]
  }

  object Definition {
    object WithTypeParams {
      def unapply(ir: IR): Option[(List[Name], DefinitionCase[TypeTree])] = ir match {
        case ir: Definition => Some((ir.typeParams, ir.caseValue))
        case _              => None
      }
    }
    final case class CustomTypeDefinition(typeParams: List[Name], ctors: AccessControlled[Constructors])
        extends Definition {
      override lazy val caseValue: DefinitionCase[TypeTree] = CustomTypeDefinitionCase(typeParams, ctors)
    }

    final case class TypeAliasDefinition(typeParams: List[Name], typeExpr: Type) extends Definition {
      override lazy val caseValue: DefinitionCase[TypeTree] = TypeAliasDefinitionCase(typeParams, typeExpr)
    }
  }

  sealed trait Specification extends TypeTree { self =>
    def typeParams: List[Name]
    override def caseValue: SpecificationCase[TypeTree]
  }
  object Specification {
    def unapply(t: Specification): Option[(SpecificationCase[TypeTree])] =
      Some(t.caseValue)

    object WithTypeParams {
      def unapply(ir: IR): Option[(List[Name], SpecificationCase[TypeTree])] = ir match {
        case s: Specification => Some((s.typeParams, s.caseValue))
        case _                => None
      }
    }

    final case class CustomTypeSpecification(typeParams: List[Name], ctors: Constructors) extends Specification {
      override lazy val caseValue: SpecificationCase[TypeTree] = CustomTypeSpecificationCase(typeParams, ctors)
    }
    final case class OpaqueTypeSpecification(typeParams: List[Name]) extends Specification {
      override lazy val caseValue: SpecificationCase[Type] = OpaqueTypeSpecificationCase(typeParams)
    }

    final case class TypeAliasSpecification(typeParams: List[Name], typeExpr: Type) extends Specification {
      override lazy val caseValue: SpecificationCase[Type] = TypeAliasSpecificationCase(typeParams, typeExpr)
    }
  }
}

sealed trait Type extends TypeTree { self =>
  // import TypeCase.*

  final def asType: Type = self

  override def caseValue: TypeCase[Type]
}

object Type {
  import TypeCase.*

  def ref(name: naming.FQName): Reference = Reference(name, Chunk.empty)

  /**
   * Creates a type variable with the given `name`.
   */
  def variable(name: Name): Variable   = Variable(name)
  def variable(name: String): Variable = variable(Name(name))
  val unit: Type                       = UnitType

  case object UnitType extends Type {
    override val caseValue: TypeCase[Type] = UnitCase
  }

  final case class Field private (caseValue: FieldCase[Type]) extends Type
  object Field {
    def apply(name: Name, fieldType: Type): Field =
      Field(FieldCase(name, fieldType))
    def unapply(field: Field): Option[(Name, Type)] =
      Some((field.caseValue.name, field.caseValue.fieldType))

    object Case {
      def unapply(field: Field): Option[FieldCase[Type]] =
        Some(field.caseValue)
    }
  }

  final case class Reference(name: FQName, typeParams: Chunk[Type]) extends Type {
    override lazy val caseValue: ReferenceCase[Type] = ReferenceCase(name, typeParams)
  }

  object Reference {
    object Case {
      def unapply(reference: Reference): Option[ReferenceCase[Type]] =
        Some(reference.caseValue)
    }
  }

  final case class Variable(name: Name) extends Type {
    override lazy val caseValue: VariableCase = VariableCase(name)
  }
  object Variable {
    object Case {
      def unapply(variable: Variable): Option[VariableCase] =
        Some(variable.caseValue)
    }
  }
}
sealed trait ValueTree extends IR { self =>
  import ValueTreeCase.*
  import ValueCase.*
  override def caseValue: ValueTreeCase[IR]
}

object ValueTree {
  import ValueTreeCase.*

  final case class Definition(inputTypes: Chunk[(Name, Type)], outputType: Type, body: Value) extends ValueTree {
    override def caseValue: ValueTreeCase[IR] = DefinitionCase(inputTypes, outputType, body)
  }

  final case class Specification(inputs: Chunk[(Name, Type)], output: Type) extends ValueTree {
    override val caseValue: ValueTreeCase[IR] = SpecificationCase(inputs, output)
  }
}

sealed trait Value extends ValueTree { self =>

  def caseValue: ValueCase[Value]
}

object Value {
  import ValueCase.*
  final case class Variable(name: Name) extends Value {
    override lazy val caseValue: VariableCase = VariableCase(name)
  }
}

sealed trait Distribution extends IR {
  def caseValue: DistributionCase[IR]
}

object Distribution {
  import DistributionCase.*
  final case class Library(
      packageName: PackageName,
      packageSpecs: Map[PackageName, PackageSpecification],
      packageDef: PackageDefinition
  ) extends Distribution {
    override def caseValue: LibraryCase[IR] = LibraryCase(packageName, packageSpecs, packageDef)
  }
}

final case class PackageSpecification(modules: Map[ModuleName, ModuleSpecification]) extends IR {
  override def caseValue: PackageSpecificationCase[ModuleSpecification] = PackageSpecificationCase(modules)
}

final case class PackageDefinition(modules: Map[ModuleName, AccessControlled[ModuleDefinition]]) extends IR {
  override def caseValue: PackageDefinitionCase[ModuleDefinition] = PackageDefinitionCase(modules)
}

final case class ModuleDefinition(
    types: Map[Name, AccessControlled[Documented[TypeTree.Definition]]],
    values: Map[Name, AccessControlled[ValueTree.Definition]]
) extends IR {
  override def caseValue: ModuleDefinitionCase[IR] = ModuleDefinitionCase(types, values)
}

final case class ModuleSpecification(
    types: Map[Name, Documented[TypeTree.Specification]],
    values: Map[Name, ValueTree.Specification]
) extends IR {
  override def caseValue: ModuleSpecificationCase[IR] = ModuleSpecificationCase(types, values)
}

sealed trait Literal[+A] {
  def value: A
}
object Literal {
  final case class Bool(value: scala.Boolean)               extends Literal[scala.Boolean]
  final case class Char(value: scala.Char)                  extends Literal[scala.Char]
  final case class String(value: java.lang.String)          extends Literal[java.lang.String]
  final case class WholeNumber(value: java.math.BigInteger) extends Literal[java.math.BigInteger]
  // TODO: Consider using BigDecimal as the representation of Float in Literal
  final case class Float(value: java.math.BigDecimal) extends Literal[java.math.BigDecimal]
}

sealed trait IR { self =>
  import IRCase.*

  def caseValue: IRCase[IR]

  def fold[Z](f: IRCase[Z] => Z): Z =
    self.caseValue match {
      case c @ DistributionCase.LibraryCase(_, _, _) =>
        f(
          DistributionCase.LibraryCase(
            c.packageName,
            c.packageSpecs.map { case (name, spec) => (name, spec.fold(f)) },
            c.packageDef.fold(f)
          )
        )
      case c @ ModuleDefinitionCase(_, _) =>
        f(
          ModuleDefinitionCase(
            c.types.map { case (name, value) => (name, value.map(d => d.map(_.fold(f)))) },
            c.values.map { case (name, value) => (name, value.map(_.fold(f))) }
          )
        )
      case c @ ModuleSpecificationCase(_, _) =>
        f(
          ModuleSpecificationCase(
            c.types.map { case (name, value) => (name, value.map(_.fold(f))) },
            c.values.map { case (name, value) => (name, value.fold(f)) }
          )
        )
      case c @ PackageDefinitionCase(_) =>
        f(PackageDefinitionCase(c.modules.map { case (name, value) => (name, value.map(_.fold(f))) }))
      case c @ PackageSpecificationCase(_) =>
        f(PackageSpecificationCase(c.modules.map { case (name, spec) => (name, spec.fold(f)) }))
      case c @ PatternCase.AsCase(_, _) => f(PatternCase.AsCase(c.pattern.fold(f), c.name))
      case c @ PatternCase.ConstructorCase(_, _) =>
        f(PatternCase.ConstructorCase(c.constructorName, c.argumentPatterns.map(_.fold(f))))
      case c @ PatternCase.EmptyListCase      => f(PatternCase.EmptyListCase)
      case c @ PatternCase.HeadTailCase(_, _) => f(PatternCase.HeadTailCase(c.head.fold(f), c.tail.fold(f)))
      case c @ PatternCase.LiteralCase(_)     => f(PatternCase.LiteralCase(c.value))
      case c @ PatternCase.TupleCase(_)       => f(PatternCase.TupleCase(c.elements.map(_.fold(f))))
      case c @ PatternCase.UnitCase           => f(PatternCase.UnitCase)
      case c @ PatternCase.WildcardCase       => f(PatternCase.WildcardCase)
      case c @ ValueCase.ApplyCase(_, _)      => f(ValueCase.ApplyCase(c.function.fold(f), c.arguments.map(_.fold(f))))
      case c @ ValueCase.ConstructorCase(_) =>
        f(c)
        f(ValueCase.ConstructorCase(c.name))
      case c @ ValueCase.DestructureCase(_, _, _) =>
        f(ValueCase.DestructureCase(c.pattern.fold(f), c.valueToDestruct.fold(f), c.inValue.fold(f)))
      case c @ ValueCase.FieldCase(_, _)      => f(ValueCase.FieldCase(c.target.fold(f), c.name))
      case c @ ValueCase.FieldFunctionCase(_) => f(c)
      case c @ ValueCase.IfThenElseCase(_, _, _) =>
        f(ValueCase.IfThenElseCase(c.condition.fold(f), c.thenBranch.fold(f), c.elseBranch.fold(f)))
      case c @ ValueCase.LambdaCase(_, _) => f(ValueCase.LambdaCase(c.argumentPattern.fold(f), c.body.fold(f)))
      case c @ ValueCase.LetDefinitionCase(_, _, _) =>
        f(ValueCase.LetDefinitionCase(c.valueName, c.valueDefinition.fold(f), c.inValue.fold(f)))
      case c @ ValueCase.LetRecursionCase(_, _) =>
        f(
          ValueCase.LetRecursionCase(
            c.valueDefinitions.map { case (name, value) => (name, value.fold(f)) },
            c.inValue.fold(f)
          )
        )
      case c @ ValueCase.ListCase(_)    => f(ValueCase.ListCase(c.elements.map(_.fold(f))))
      case c @ ValueCase.LiteralCase(_) => f(c)
      case c @ ValueCase.PatternMatchCase(_, _) =>
        f(
          ValueCase.PatternMatchCase(
            c.branchOutOn.fold(f),
            c.cases.map { case (pattern, value) =>
              (pattern.fold(f), value.fold(f))
            }
          )
        )
      case c @ ValueCase.RecordCase(_)    => f(ValueCase.RecordCase(c.fields.map { case (k, v) => (k, v.fold(f)) }))
      case c @ ValueCase.ReferenceCase(_) => f(c)
      case c @ ValueCase.TupleCase(_)     => f(ValueCase.TupleCase(c.elements.map(_.fold(f))))
      case _ @ValueCase.UnitCase          => f(ValueCase.UnitCase)
      case c @ ValueCase.UpdateRecordCase(_, _) =>
        f(
          ValueCase.UpdateRecordCase(
            c.valueToUpdate.fold(f),
            c.fieldsToUpdate.map { case (name, value) => (name, value.fold(f)) }
          )
        )
      case c @ ValueCase.VariableCase(_)             => f(c)
      case c @ ValueTreeCase.DefinitionCase(_, _, _) => ???
      case c @ ValueTreeCase.SpecificationCase(_, _) =>
        f(
          ValueTreeCase.SpecificationCase(
            c.inputs.map { case (name, value) => (name, value.fold(f)) },
            c.output.fold(f)
          )
        )
      case c @ TypeCase.ExtensibleRecordCase(_, _) => f(TypeCase.ExtensibleRecordCase(c.name, c.fields.map(_.fold(f))))
      case c @ TypeCase.FieldCase(_, _)            => f(TypeCase.FieldCase(c.name, c.fieldType.fold(f)))
      case c @ TypeCase.FunctionCase(_, _) =>
        f(TypeCase.FunctionCase(c.paramTypes.map(_.fold(f)), c.returnType.fold(f)))
      case c @ TypeCase.RecordCase(_)       => f(TypeCase.RecordCase(c.fields.map(_.fold(f))))
      case c @ TypeCase.ReferenceCase(_, _) => f(TypeCase.ReferenceCase(c.typeName, c.typeParams.map(_.fold(f))))
      case c @ TypeCase.TupleCase(_)        => f(TypeCase.TupleCase(c.elementTypes.map(_.fold(f))))
      case c @ TypeCase.UnitCase            => f(TypeCase.UnitCase)
      case c @ TypeCase.VariableCase(_)     => f(c)
      case c @ TypeTreeCase.ConstructorsCase(_) =>
        f(TypeTreeCase.ConstructorsCase(c.args.map { case (name, tree) => (name, tree.fold(f)) }))
      case c @ TypeTreeCase.DefinitionCase.CustomTypeDefinitionCase(_, _) =>
        f(TypeTreeCase.DefinitionCase.CustomTypeDefinitionCase(c.typeParams, c.ctors.map(_.fold(f))))
      case c @ TypeTreeCase.DefinitionCase.TypeAliasDefinitionCase(_, _) =>
        f(TypeTreeCase.DefinitionCase.TypeAliasDefinitionCase(c.typeParams, c.typeExpr.fold(f)))
      case c @ TypeTreeCase.SpecificationCase.CustomTypeSpecificationCase(_, _) =>
        f(TypeTreeCase.SpecificationCase.CustomTypeSpecificationCase(c.typeParams, c.ctors.fold(f)))
      case c @ TypeTreeCase.SpecificationCase.OpaqueTypeSpecificationCase(_) => f(c)
      case c @ TypeTreeCase.SpecificationCase.TypeAliasSpecificationCase(_, _) =>
        f(TypeTreeCase.SpecificationCase.TypeAliasSpecificationCase(c.typeParams, c.typeExpr.fold(f)))
    }

  /**
   * Folds over the recursive data structure to reduce it to a summary value, providing access to the recursive
   * structure annotated with the current previous summary values in each step of the fold.
   */
  def foldAttributed[Z](f: IRCase[Attributed[IRCase, Z]] => Z): Z = {
    def annotate(recursive: IR): Attributed[IRCase, Z] =
      Attributed(recursive.caseValue.map(annotate), recursive.foldAttributed(f))
    f(caseValue.map(annotate))
  }
}
object IR {}
// final case class Field[+A](name: Name, fieldType: TypeCase[A]) { self =>
//   def map[B](f: A => B): Field[B] = copy(fieldType = fieldType.map(f))
// }

// object Field {
//   implicit val FieldCovariant: Covariant[Field] = new Covariant[Field] {
//     def map[A, B](fa: Field[A])(f: A => B): Field[B] = fa.map(f)
//   }
// }

// final case class Fields[+A](items: zio.Chunk[Field[A]]) { self =>
//   def map[B](f: A => B): Fields[B] = copy(items = items.map(_.map(f)))
// }

// object Fields {
//   implicit val FieldsCovariant: Covariant[Fields] = new Covariant[Fields] {
//     def map[A, B](fa: Fields[A])(f: A => B): Fields[B] = fa.map(f)
//   }
// }
