package zio.morphir.ir

import zio.morphir.ir.{Literal => Lit}
import zio.{Chunk, ZEnvironment}
import zio.prelude.*

object ValueModule {

  sealed trait Pattern[+Annotations] {
    def annotations: ZEnvironment[Annotations]
  }
  object Pattern {
    val unit: Unit[Any]                                                              = Unit(ZEnvironment.empty)
    def unit[Annotations](annotations: ZEnvironment[Annotations]): Unit[Annotations] = Unit(annotations)
    val wildcard: Wildcard[Any]                                                      = Wildcard(ZEnvironment.empty)
    def wildcard[Annotations](annotations: ZEnvironment[Annotations]): Wildcard[Annotations] = Wildcard(annotations)

    final case class Literal[+Annotations, +Value](value: Lit[Value], annotations: ZEnvironment[Annotations])
        extends Pattern[Annotations]

    final case class Unit[+Annotations](annotations: ZEnvironment[Annotations])     extends Pattern[Annotations]
    final case class Wildcard[+Annotations](annotations: ZEnvironment[Annotations]) extends Pattern[Annotations]
  }

  sealed trait Value[+Annotations] {
    def annotations: ZEnvironment[Annotations]
    def caseValue: ValueCase[Value[Annotations]]
  }

  object Value {
    import ValueCase.*

    final case class Literal[+V, +Annotations](value: Lit[V], annotations: ZEnvironment[Annotations])
        extends Value[Annotations] {
      def caseValue: LiteralCase = ValueCase.LiteralCase(value)
    }

    final case class Unit[+Annotations](annotations: ZEnvironment[Annotations]) extends Value[Annotations] {
      override def caseValue: UnitCase = ValueCase.UnitCase
    }
  }

  sealed trait ValueCase[+Self] { self =>
    import ValueCase.*
    def map[B](f: Self => B): ValueCase[B] = self match {
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
    final case class NativeApplyCase[+Self](function: NativeFunction, arguments: Chunk[Self]) extends ValueCase[Self]
    final case class ApplyCase[+Self](function: Self, arguments: List[Self])                  extends ValueCase[Self]
    final case class ConstructorCase(name: FQName)                                            extends ValueCase[Nothing]
    final case class FieldCase[+Self](target: Self, name: Name)                               extends ValueCase[Self]
    final case class FieldFunctionCase(name: Name)                                            extends ValueCase[Nothing]
    final case class IfThenElseCase[+Self](condition: Self, thenBranch: Self, elseBranch: Self) extends ValueCase[Self]
    final case class ListCase[+Self](elements: Chunk[Self])                                     extends ValueCase[Self]
    final case class LiteralCase(literal: LiteralValue)                                    extends ValueCase[Nothing]
    final case class PatternMatchCase[+Self](branchOutOn: Self, cases: List[(Self, Self)]) extends ValueCase[Self]
    final case class RecordCase[+Self](fields: Chunk[(Name, Self)])                        extends ValueCase[Self]
    final case class ReferenceCase(name: FQName)                                           extends ValueCase[Nothing]
    final case class TupleCase[+Self](elements: Chunk[Self])                               extends ValueCase[Self]
    case object UnitCase                                                                   extends ValueCase[Nothing]
    type UnitCase = UnitCase.type
    final case class VariableCase(name: Name) extends ValueCase[Nothing]
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

    implicit val ValueCaseForEach: ForEach[ValueCase] =
      new ForEach[ValueCase] {
        def forEach[G[+_]: IdentityBoth: Covariant, A, B](fa: ValueCase[A])(f: A => G[B]): G[ValueCase[B]] =
          fa match {
            case c @ ApplyCase(_, _)    => f(c.function).zipWith(c.arguments.forEach(f))(ApplyCase(_, _))
            case c @ ConstructorCase(_) => c.succeed
            case c @ DestructureCase(_, _, _) =>
              (f(c.pattern), f(c.valueToDestruct), f(c.inValue)).mapN(DestructureCase(_, _, _))
            case c @ FieldCase(_, _)      => f(c.target).map(FieldCase(_, c.name))
            case c @ FieldFunctionCase(_) => c.succeed
            case c @ IfThenElseCase(_, _, _) =>
              (f(c.condition), f(c.thenBranch), f(c.elseBranch)).mapN(IfThenElseCase(_, _, _))
            case c @ LambdaCase(_, _) => f(c.argumentPattern).zipWith(f(c.body))(LambdaCase(_, _))
            case c @ LetDefinitionCase(_, _, _) =>
              f(c.valueDefinition).zipWith(f(c.inValue))(LetDefinitionCase(c.valueName, _, _))
            case c @ LetRecursionCase(_, _) =>
              c.valueDefinitions.forEach(f).zipWith(f(c.inValue))(LetRecursionCase(_, _))
            case c @ ListCase(_)    => c.elements.forEach(f).map(ListCase(_))
            case c @ LiteralCase(_) => c.succeed
            case c @ PatternMatchCase(_, _) =>
              f(c.branchOutOn)
                .zipWith(c.cases.forEach { case (key, value) => f(key).zip(f(value)) })(PatternMatchCase(_, _))
            case c @ RecordCase(_) =>
              c.fields.forEach { case (key, value) => f(value).map(key -> _) }.map(RecordCase(_))
            case c @ ReferenceCase(_) => c.succeed
            case c @ TupleCase(_)     => c.elements.forEach(f).map(TupleCase(_))
            case _ @UnitCase          => UnitCase.succeed
            case c @ UpdateRecordCase(_, _) =>
              f(c.valueToUpdate).zipWith(c.fieldsToUpdate.forEach { case (name, self) => f(self).map(name -> _) })(
                UpdateRecordCase(_, _)
              )
            case c @ VariableCase(_) => c.succeed
          }
      }
  }

}
