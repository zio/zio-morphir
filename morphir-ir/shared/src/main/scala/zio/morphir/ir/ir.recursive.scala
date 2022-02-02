package zio.morphir.ir
import zio.Chunk
import zio.prelude.*
object recursive {

  sealed trait IRCase[+Self] { self =>

    def map[B](f: Self => B): IRCase[B] =
      self match {      
        case c: DistributionCase[s]         => c.map(f)
        case c: ModuleDefinitionCase[s]     => c.map(f)
        case c: ModuleSpecificationCase[s]  => c.map(f)
        case c: PatternCase[s]              => c.map(f)
        case c: PackageSpecificationCase[s] => c.map(f)      
        case c: PackageDefinitionCase[s]    => c.map(f)   
        case c: TypeTreeCase[s]             => c.map(f)
        case c: ValueTreeCase[s]            => c.map(f)
      }
  }
  object IRCase {
    type TypeCase[+Self] = zio.morphir.ir.recursive.TypeCase[Self]
    val TypeCase = zio.morphir.ir.recursive.TypeCase

    type ValueCase[+Self] = zio.morphir.ir.recursive.ValueCase[Self]
    val ValueCase = zio.morphir.ir.recursive.ValueCase

    type PackageSpecificationCase[+Self] = zio.morphir.ir.recursive.PackageSpecificationCase[Self]
    val PackageSpecificationCase = zio.morphir.ir.recursive.PackageSpecificationCase

    type ModuleSpecificationCase[+Self] = zio.morphir.ir.recursive.ModuleSpecificationCase[Self]
    val ModuleSpecificationCase = zio.morphir.ir.recursive.ModuleSpecificationCase

    type ModuleDefinitionCase[+Self] = zio.morphir.ir.recursive.ModuleDefinitionCase[Self]
    val ModuleDefinitionCase = zio.morphir.ir.recursive.ModuleDefinitionCase
  }

  sealed trait DistributionCase[+Self] extends IRCase[Self] { self => 
    import DistributionCase.*

    override def map[B](f: Self => B): DistributionCase[B] =
      self match {
        case c @ LibraryCase(_,_,_) => LibraryCase(c.packageName, c.packageSpecs.map { case (name, spec) => (name, f(spec)) }, f(c.packageDef))
      }  
  }
  object DistributionCase {
    final case class LibraryCase[+Self](packageName : PackageName, packageSpecs: Map[PackageName, Self], packageDef : Self) extends DistributionCase[Self]    
  }
  
  final case class PackageSpecificationCase[+Self](modules: Map[ModuleName, Self]) extends IRCase[Self]
  final case class PackageDefinitionCase[+Self](modules: Map[ModuleName, AccessControlled[Self]]) extends IRCase[Self]
  final case class ModuleSpecificationCase[+Self](types:Map[Name, Documented[Self]], values:Map[Name, Self]) extends IRCase[Self]
  final case class ModuleDefinitionCase[+Self](types: Map[Name, AccessControlled[Documented[Self]]], values: Map[Name, AccessControlled[Self]]) extends IRCase[Self]

  sealed trait TypeTreeCase[+Self] extends IRCase[Self] { self =>
    import TypeTreeCase.*
    import DefinitionCase.*
    import SpecificationCase.*

    override def map[B](f: Self => B): TypeTreeCase[B] =
      self match {
        case c @ ConstructorsCase(_) => ConstructorsCase(c.args.map { case (name, argType) => (name, f(argType)) })
        case c @ CustomTypeDefinitionCase(_, _)    => CustomTypeDefinitionCase(c.typeParams, c.ctors.map(f))
        case c @ CustomTypeSpecificationCase(_, _) => CustomTypeSpecificationCase(c.typeParams, f(c.ctors))
        case c @ OpaqueTypeSpecificationCase(_)    => c
        case c @ TypeAliasDefinitionCase(_, _)     => TypeAliasDefinitionCase(c.typeParams, f(c.typeExpr))
        case c @ TypeAliasSpecificationCase(_, _)  => TypeAliasSpecificationCase(c.typeParams, f(c.typeExpr))
        case c: TypeExprCase[s]                    => c.map(f)
      }
  }
  object TypeTreeCase {
    final case class ConstructorsCase[+Self](args: Map[Name, Self]) extends TypeTreeCase[Self]
    sealed trait StatementCase[+Self]                               extends TypeTreeCase[Self]

    sealed trait DefinitionCase[+Self] extends StatementCase[Self]
    object DefinitionCase {
      final case class CustomTypeDefinitionCase[+Self](typeParams: List[Name], ctors: AccessControlled[Self])
          extends DefinitionCase[Self]
      final case class TypeAliasDefinitionCase[+Self](typeParams: List[Name], typeExpr: Self)
          extends DefinitionCase[Self]
    }

    sealed trait SpecificationCase[+Self] extends StatementCase[Self] {
      def typeParams: List[Name]
    }

    object SpecificationCase {
      object TypeParams {
        def unapply[Self](irCase: IRCase[Self]): Option[List[Name]] = irCase match {
          case c: SpecificationCase[s] => Some(c.typeParams)
          case _                       => None
        }
      }
      final case class CustomTypeSpecificationCase[+Self](typeParams: List[Name], ctors: Self)
          extends SpecificationCase[Self]
      final case class OpaqueTypeSpecificationCase(typeParams: List[Name]) extends SpecificationCase[Nothing]
      final case class TypeAliasSpecificationCase[+Self](typeParams: List[Name], typeExpr: Self)
          extends SpecificationCase[Self]
    }

    type TypeExprCase[+Self] = zio.morphir.ir.recursive.TypeCase[Self]
    val TypeExprCase = zio.morphir.ir.recursive.TypeCase
  }

  sealed trait TypeCase[+Self] extends TypeTreeCase[Self] { self =>
    import TypeCase.*
    override def map[B](f: Self => B): TypeCase[B] = self match {
      case c @ ExtensibleRecordCase(_, _) => ExtensibleRecordCase(c.name, c.fields.map(f))
      case c @ FieldCase(_, _)            => FieldCase(c.name, f(c.fieldType))
      case c @ FunctionCase(_, _)         => FunctionCase(c.paramTypes.map(f), f(c.returnType))
      case c @ ReferenceCase(_, _)        => ReferenceCase(c.typeName, c.typeParams.map(f))
      case c @ TupleCase(_)               => TupleCase(c.elementTypes.map(f))
      case UnitCase                       => UnitCase
      case c @ VariableCase(_)            => VariableCase(c.name)
      case c @ RecordCase(_)              => RecordCase(c.fields.map(f))
    }
  }

  object TypeCase {
    final case class ExtensibleRecordCase[+Self](name: Name, fields: Chunk[Self])    extends TypeCase[Self]
    final case class FunctionCase[+Self](paramTypes: List[Self], returnType: Self)   extends TypeCase[Self]
    final case class RecordCase[+Self](fields: Chunk[Self])                          extends TypeCase[Self]
    final case class ReferenceCase[+Self](typeName: FQName, typeParams: Chunk[Self]) extends TypeCase[Self]
    final case class TupleCase[+Self](elementTypes: List[Self])                      extends TypeCase[Self]
    case object UnitCase                                                             extends TypeCase[Nothing]
    final case class VariableCase(name: Name)                                        extends TypeCase[Nothing]
    final case class FieldCase[+Self](name: Name, fieldType: Self)                   extends TypeCase[Self]

    implicit val TypeCaseCovariant: Covariant[TypeCase] = new Covariant[TypeCase] {
      def map[A, B](f: A => B): TypeCase[A] => TypeCase[B] = _.map(f)
    }
  }

  sealed trait ValueTreeCase[+Self] extends IRCase[Self] { self =>
    import ValueTreeCase.*

    override def map[B](f: Self => B): ValueTreeCase[B] =
      self match {
        case c @ DefinitionCase(_, _, _) =>
          DefinitionCase(c.inputTypes.map { case (name, self) => (name, f(self)) }, f(c.outputType), f(c.body))
        case c @ SpecificationCase(_, _) =>
          SpecificationCase(c.inputs.map { case (name, self) => (name, f(self)) }, f(c.output))
        case c: ValueCase[s] => c.map(f)
      }
  }

  object ValueTreeCase {

    final case class DefinitionCase[+Self](inputTypes: Chunk[(Name, Self)], outputType: Self, body: Self)
        extends ValueTreeCase[Self]

    final case class SpecificationCase[+Self](inputs: Chunk[(Name, Self)], output: Self) extends ValueTreeCase[Self]
  }

  sealed trait ValueCase[+Self] extends ValueTreeCase[Self] { self =>
    import ValueCase.*
    override def map[B](f: Self => B): ValueCase[B] = self match {
      case c @ ApplyCase(_, _)          => ApplyCase(f(c.function), c.arguments.map(f))
      case c @ ConstructorCase(_)       => ConstructorCase(c.name)
      case c @ DestructureCase(_, _, _) => DestructureCase(f(c.pattern), f(c.valueToDestruct), f(c.inValue))
      case c @ FieldCase(_, _)          => FieldCase(f(c.target), c.name)
      case c @ FieldFunctionCase(_)     => FieldFunctionCase(c.name)
      case c @ IfThenElseCase(_, _, _) =>
        IfThenElseCase(f(c.condition), f(c.thenBranch), f(c.elseBranch))
      case c @ LambdaCase(_, _)           => LambdaCase(f(c.argumentPattern), f(c.body))
      case c @ LetDefinitionCase(_, _, _) => LetDefinitionCase(c.valueName, f(c.valueDefinition), f(c.inValue))
      case c @ LetRecursionCase(_, _) =>
        LetRecursionCase(c.valueDefinitions.map { case (name, value) => (name, f(value)) }, f(c.inValue))
      case c @ ListCase(_)    => ListCase(c.elements.map(f))
      case c @ LiteralCase(_) => LiteralCase(c.literal)
      case c @ PatternMatchCase(_, _) =>
        PatternMatchCase(f(c.branchOutOn), c.cases.map { case (p, v) => (f(p), f(v)) })
      case c @ RecordCase(_)    => RecordCase(c.fields.map { case (name, value) => (name, f(value)) })
      case c @ ReferenceCase(_) => c
      case c @ TupleCase(_)     => TupleCase(c.elements.map(f))
      case _ @UnitCase          => UnitCase
      case c @ UpdateRecordCase(_, _) =>
        UpdateRecordCase(f(c.valueToUpdate), c.fieldsToUpdate.map { case (name, self) => (name, f(self)) })
      case c @ VariableCase(_) => c

    }
  }
  object ValueCase {
    final case class ApplyCase[+Self](function: Self, arguments: List[Self]) extends ValueCase[Self]
    final case class ConstructorCase(name: FQName)                           extends ValueCase[Nothing]
    final case class FieldCase[+Self](target: Self, name: Name)              extends ValueCase[Self]
    final case class FieldFunctionCase(name: Name)                           extends ValueCase[Nothing]
    final case class IfThenElseCase[+Self](condition: Self, thenBranch: Self, elseBranch: Self) extends ValueCase[Self]
    final case class ListCase[+Self](elements: List[Self])                                      extends ValueCase[Self]
    final case class LiteralCase(literal: LiteralValue)                                    extends ValueCase[Nothing]
    final case class PatternMatchCase[+Self](branchOutOn: Self, cases: List[(Self, Self)]) extends ValueCase[Self]
    final case class RecordCase[+Self](fields: List[(Name, Self)])                         extends ValueCase[Self]
    final case class ReferenceCase(name: FQName)                                           extends ValueCase[Nothing]
    final case class TupleCase[+Self](elements: List[Self])                                extends ValueCase[Self]
    case object UnitCase                                                                   extends ValueCase[Nothing]
    final case class VariableCase(name: Name)                                              extends ValueCase[Nothing]
    final case class LetDefinitionCase[+Self](valueName: Name, valueDefinition: Self, inValue: Self)
        extends ValueCase[Self]
    final case class LetRecursionCase[+Self](valueDefinitions: Map[Name, Self], inValue: Self) extends ValueCase[Self]
    final case class UpdateRecordCase[+Self](valueToUpdate: Self, fieldsToUpdate: Chunk[(Name, Self)])
        extends ValueCase[Self]
    final case class LambdaCase[+Self](argumentPattern: Self, body: Self)                        extends ValueCase[Self]
    final case class DestructureCase[+Self](pattern: Self, valueToDestruct: Self, inValue: Self) extends ValueCase[Self]

    implicit val ValueCaseCovariant: Covariant[ValueCase] = new Covariant[ValueCase] {
      def map[A, B](f: A => B): ValueCase[A] => ValueCase[B] = _.map(f)
    }
  }

  sealed trait PatternCase[+Self] extends IRCase[Self] { self =>
    import PatternCase.*

    override def map[B](f: Self => B): PatternCase[B] = self match {
      case c @ AsCase(_, _)          => AsCase(f(c.pattern), c.name)
      case c @ ConstructorCase(_, _) => ConstructorCase(c.constructorName, c.argumentPatterns.map(f))
      case EmptyListCase             => EmptyListCase
      case c @ HeadTailCase(_, _)    => HeadTailCase(f(c.head), f(c.tail))
      case c @ LiteralCase(_)        => c
      case c @ TupleCase(_)          => TupleCase(c.elements.map(f))
      case UnitCase                  => UnitCase
      case WildcardCase              => WildcardCase
    }

  }

  object PatternCase {
    final case class AsCase[+Self](pattern: Self, name: Name) extends PatternCase[Self]
    final case class ConstructorCase[+Self](constructorName: FQName, argumentPatterns: List[Self])
        extends PatternCase[Self]
    case object EmptyListCase                                    extends PatternCase[Nothing]
    final case class HeadTailCase[+Self](head: Self, tail: Self) extends PatternCase[Self]
    final case class LiteralCase(value: Literal[Nothing])        extends PatternCase[Nothing]
    final case class TupleCase[+Self](elements: List[Self])      extends PatternCase[Self]
    case object UnitCase                                         extends PatternCase[Nothing]
    case object WildcardCase                                     extends PatternCase[Nothing]
  }
}
