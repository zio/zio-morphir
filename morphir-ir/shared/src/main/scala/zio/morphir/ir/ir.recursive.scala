package zio.morphir.ir
import zio.Chunk

object recursive {

  sealed trait IRCase[+Self] { self => }
  object IRCase {
    type TypeCase[+Self] = zio.morphir.ir.recursive.TypeCase[Self]
    val TypeCase = zio.morphir.ir.recursive.TypeCase

    type ValueCase[+Self] = zio.morphir.ir.recursive.ValueCase[Self]
    val ValueCase = zio.morphir.ir.recursive.ValueCase
  }

  sealed trait TypeCase[+Self] extends IRCase[Self] { self =>
    import TypeCase.*
    def map[B](f: Self => B): TypeCase[B] = self match {
      case c @ ExtensibleRecordCase(_, _) => ExtensibleRecordCase(c.name, c.fields.map(f))
      case c @ FieldCase(_, _)            => FieldCase(c.name, f(c.fieldType))
      case c @ FunctionCase(_, _)         => FunctionCase(c.paramTypes.map(f), f(c.returnType))
      case c @ ReferenceCase(_, _)        => ReferenceCase(c.typeName, c.typeParams.map(f))
      case c @ TupleCase(_)               => TupleCase(c.elementTypes.map(f))
      case c @ UnitCase                   => UnitCase
      case c @ VariableCase(_)            => VariableCase(c.name)
      case c @ RecordCase(_)              => RecordCase(c.fields.map(f))
    }
  }

  object TypeCase {
    final case class ExtensibleRecordCase[+A](name: Name, fields: Chunk[A])    extends TypeCase[A]
    final case class FunctionCase[+A](paramTypes: List[A], returnType: A)      extends TypeCase[A]
    final case class RecordCase[+A](fields: Chunk[A])                          extends TypeCase[A]
    final case class ReferenceCase[+A](typeName: FQName, typeParams: Chunk[A]) extends TypeCase[A]
    final case class TupleCase[+A](elementTypes: List[A])                      extends TypeCase[A]
    case object UnitCase                                                       extends TypeCase[Nothing]
    final case class VariableCase(name: Name)                                  extends TypeCase[Nothing]
    final case class FieldCase[+A](name: Name, fieldType: A)                   extends TypeCase[A]
  }

  sealed trait ValueCase[+Self] extends IRCase[Self] { self =>
    import ValueCase.*
    def map[B](f: Self => B): ValueCase[B] = self match {
      case c @ ApplyCase(_, _)      => ApplyCase(f(c.function), c.arguments.map(f))
      case c @ ConstructorCase(_)   => ConstructorCase(c.name)
      case c @ FieldCase(_, _)      => FieldCase(f(c.target), c.name)
      case c @ FieldFunctionCase(_) => FieldFunctionCase(c.name)
      case c @ IfThenElseCase(_, _, _) =>
        IfThenElseCase(f(c.condition), f(c.thenBranch), f(c.elseBranch))
      case c @ ListCase(_)    => ListCase(c.elements.map(f))
      case c @ LiteralCase(_) => LiteralCase(c.literal)
      case c @ PatternMatchCase(_, _) =>
        PatternMatchCase(f(c.branchOutOn), c.cases.map { case (p, v) => (f(p), f(v)) })
      case c @ RecordCase(_)    => RecordCase(c.fields.map { case (name, value) => (name, f(value)) })
      case c @ ReferenceCase(_) => c
      case c @ TupleCase(_)     => TupleCase(c.elements.map(f))
      case _ @UnitCase          => UnitCase
      case c @ VariableCase(_)  => c
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
  }

  sealed trait PatternCase[+Self] extends IRCase[Self] { self =>
    import PatternCase.*

    def map[B](f: Self => B): PatternCase[B] = self match {
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
