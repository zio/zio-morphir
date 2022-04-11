package zio.morphir.ir.value.recursive

//import zio.Chunk
import zio.morphir.ir.{Literal => Lit}
import zio.morphir.ir.Type.UType
import zio.morphir.ir.value.Pattern

final case class Value[+TA, +VA](caseValue: ValueCase[TA, VA, Value[TA, VA]]) { self =>
  import ValueCase._
  def attributes: VA = caseValue.attributes

  def fold[Z](f: (ValueCase[TA, VA, Z]) => Z): Z = self.caseValue match {
    case c @ ApplyCase(_, _, _)    => f(ApplyCase(c.attributes, c.function.fold(f), c.argument.fold(f)))
    case c @ ConstructorCase(_, _) => f(c)
    case c @ DestructureCase(_, _, _, _) =>
      f(DestructureCase(c.attributes, c.pattern, c.valueToDestruct.fold(f), c.inValue.fold(f)))
    case c @ FieldCase(_, _, _)      => f(FieldCase(c.attributes, c.target.fold(f), c.name))
    case c @ FieldFunctionCase(_, _) => f(c)
    case c @ IfThenElseCase(_, _, _, _) =>
      f(IfThenElseCase(c.attributes, c.condition.fold(f), c.thenBranch.fold(f), c.elseBranch.fold(f)))
    case c @ LambdaCase(_, _, _) => f(LambdaCase(c.attributes, c.argumentPattern, c.body.fold(f)))
    case c @ LetDefinitionCase(_, _, _, _) =>
      f(LetDefinitionCase(c.attributes, c.valueName, c.valueDefinition.map(_.fold(f)), c.inValue.fold(f)))
    case c @ LetRecursionCase(_, _, _) =>
      f(
        LetRecursionCase(
          c.attributes,
          c.valueDefinitions.map { case (n, v) => (n, v.map(_.fold(f))) },
          c.inValue.fold(f)
        )
      )
    case c @ ListCase(_, _)    => f(ListCase(c.attributes, c.elements.map(_.fold(f))))
    case c @ LiteralCase(_, _) => f(c)
    case c @ PatternMatchCase(_, _, _) =>
      f(PatternMatchCase(c.attributes, c.branchOutOn.fold(f), c.cases.map { case (p, v) => (p, v.fold(f)) }))
    case c @ RecordCase(_, _)    => f(RecordCase(c.attributes, c.fields.map { case (n, v) => (n, v.fold(f)) }))
    case c @ ReferenceCase(_, _) => f(c)
    case c @ TupleCase(_, _)     => f(TupleCase(c.attributes, c.elements.map(_.fold(f))))
    case c @ UnitCase(_)         => f(c)
    case c @ UpdateRecordCase(_, _, _) =>
      f(UpdateRecordCase(c.attributes, c.valueToUpdate.fold(f), c.fieldsToUpdate.map { case (n, v) => (n, v.fold(f)) }))
    case c @ VariableCase(_, _) => f(c)
  }

  def mapAttributes[TB, VB](f: TA => TB, g: VA => VB): Value[TB, VB] = fold[Value[TB, VB]] {
    case ApplyCase(attributes, function, argument) => Value(ApplyCase(g(attributes), function, argument))
    case ConstructorCase(attributes, name)         => Value(ConstructorCase(g(attributes), name))
    case DestructureCase(attributes, pattern, valueToDestruct, inValue) =>
      Value(DestructureCase(g(attributes), pattern.map(g), valueToDestruct, inValue))
    case FieldCase(attributes, target, name) => Value(FieldCase(g(attributes), target, name))
    case FieldFunctionCase(attributes, name) => Value(FieldFunctionCase(g(attributes), name))
    case IfThenElseCase(attributes, condition, thenBranch, elseBranch) =>
      Value(IfThenElseCase(g(attributes), condition, thenBranch, elseBranch))
    case LambdaCase(attributes, argumentPattern, body) => Value(LambdaCase(g(attributes), argumentPattern.map(g), body))
    case LetDefinitionCase(attributes, valueName, valueDefinition, inValue) =>
      Value(LetDefinitionCase(g(attributes), valueName, valueDefinition.mapAttributes(f, g), inValue))
    case LetRecursionCase(attributes, valueDefinitions, inValue) =>
      Value(
        LetRecursionCase(g(attributes), valueDefinitions.map { case (n, v) => (n, v.mapAttributes(f, g)) }, inValue)
      )
    case ListCase(attributes, elements)   => Value(ListCase(g(attributes), elements))
    case LiteralCase(attributes, literal) => Value(LiteralCase(g(attributes), literal))
    case PatternMatchCase(attributes, branchOutOn, cases) =>
      Value(PatternMatchCase(g(attributes), branchOutOn, cases.map { case (p, v) => (p.map(g), v) }))
    case RecordCase(attributes, fields)  => Value(RecordCase(g(attributes), fields))
    case ReferenceCase(attributes, name) => Value(ReferenceCase(g(attributes), name))
    case TupleCase(attributes, elements) => Value(TupleCase(g(attributes), elements))
    case UnitCase(attributes)            => Value(UnitCase(g(attributes)))
    case UpdateRecordCase(attributes, valueToUpdate, fieldsToUpdate) =>
      Value(UpdateRecordCase(g(attributes), valueToUpdate, fieldsToUpdate))
    case VariableCase(attributes, name) => Value(VariableCase(g(attributes), name))
  }

  // def mapTypeAttributes[TB](f: TA => TB): ValueCase[TB, VA, Self] = self.caseValue match {
  //   case ApplyCase(attributes, function, argument)                          => ???
  //   case ConstructorCase(attributes, name)                                  => ???
  //   case DestructureCase(attributes, pattern, valueToDestruct, inValue)     => ???
  //   case FieldCase(attributes, target, name)                                => ???
  //   case FieldFunctionCase(attributes, name)                                => ???
  //   case IfThenElseCase(attributes, condition, thenBranch, elseBranch)      => ???
  //   case LambdaCase(attributes, argumentPattern, body)                      => ???
  //   case LetDefinitionCase(attributes, valueName, valueDefinition, inValue) => ???
  //   case LetRecursionCase(attributes, valueDefinitions, inValue)            => ???
  //   case ListCase(attributes, elements)                                     => ???
  //   case LiteralCase(attributes, literal)                                   => ???
  //   case PatternMatchCase(attributes, branchOutOn, cases)                   => ???
  //   case RecordCase(attributes, fields)                                     => ???
  //   case ReferenceCase(attributes, name)                                    => ???
  //   case TupleCase(attributes, elements)                                    => ???
  //   case UnitCase(attributes)   => ???
  //   case UpdateRecordCase(attributes, valueToUpdate, fieldsToUpdate)        => ???
  //   case VariableCase(attributes, name)                                     => ???
  // }

  // def mapValueAttributes[VB](f: VA => VB): ValueCase[TA, VB, Self] = self match {
  //   case ApplyCase(attributes, function, argument)                          => ???
  //   case ConstructorCase(attributes, name)                                  => ???
  //   case DestructureCase(attributes, pattern, valueToDestruct, inValue)     => ???
  //   case FieldCase(attributes, target, name)                                => ???
  //   case FieldFunctionCase(attributes, name)                                => ???
  //   case IfThenElseCase(attributes, condition, thenBranch, elseBranch)      => ???
  //   case LambdaCase(attributes, argumentPattern, body)                      => ???
  //   case LetDefinitionCase(attributes, valueName, valueDefinition, inValue) => ???
  //   case LetRecursionCase(attributes, valueDefinitions, inValue)            => ???
  //   case ListCase(attributes, elements)                                     => ???
  //   case LiteralCase(attributes, literal)                                   => ???
  //   case PatternMatchCase(attributes, branchOutOn, cases)                   => ???
  //   case RecordCase(attributes, fields)                                     => ???
  //   case ReferenceCase(attributes, name)                                    => ???
  //   case TupleCase(attributes, elements)                                    => ???
  //   case UnitCase(attributes)                                               => ???
  //   case UpdateRecordCase(attributes, valueToUpdate, fieldsToUpdate)        => ???
  //   case VariableCase(attributes, name)                                     => ???
  // }
}

object Value {
  import ValueCase._

  type RawValue = Value[Any, Any]
  val RawValue: Value.type = Value

  type TypedValue = Value[Any, UType]
  val TypedValue: Value.type = Value

  def apply[TA, VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA]): Value[TA, VA] =
    Value(ApplyCase(attributes, function, argument))

  object Lambda {
    def apply[TA, VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA]): Value[TA, VA] =
      Value(LambdaCase(attributes, argumentPattern, body))
  }

  object Literal {
    def apply[VA, A](attributes: VA, literal: Lit[A]): Value[Nothing, VA] =
      Value(LiteralCase(attributes, literal))
  }
}
