package zio.morphir.ir.value

import zio.prelude.AnyType
import zio.{Chunk}
import zio.morphir.ir.ZEnvironmentSubset
import zio.morphir.ir.types.{Type, UType}
import zio.morphir.ir.{FQName, InferredTypeOf, Name, NativeFunction, Literal => Lit}

import scala.annotation.tailrec

sealed trait Value[+Caps[_], +TA, +VA] { self =>
  import Value.{List => ListType, Unit => UnitType, _}

  def @@[LowerTA >: TA, UpperTA >: LowerTA, LowerVA >: VA, UpperVA >: LowerVA](
      aspect: ValueAspect[LowerTA, UpperTA, LowerVA, UpperVA]
  ): Value[Caps, LowerTA, LowerVA] =
    aspect(self)

  def attributes: ZEnvironmentSubset[Caps, VA]

  def mapAttributes[TB, VB](f: TA => TB, g: ZEnvironmentSubset[Caps, VA] => ZEnvironmentSubset[Caps, VB]): Value[Caps, TB, VB] = self match {
    case t @ Apply(_, _, _) =>
      Apply(g(t.attributes), t.function.mapAttributes(f, g), t.arguments.map(_.mapAttributes(f, g)))
    case t @ Constructor(_, _) => Constructor(g(t.attributes), t.name)
    case t @ Destructure(_, _, _, _) =>
      Destructure(
        g(t.attributes),
        t.pattern.mapAttributes(g),
        t.valueToDestruct.mapAttributes(f, g),
        t.inValue.mapAttributes(f, g)
      )
    case t @ Field(_, _, _)      => Field(g(t.attributes), t.target.mapAttributes(f, g), t.name)
    case t @ FieldFunction(_, _) => FieldFunction(g(t.attributes), t.name)
    case t @ IfThenElse(_, _, _, _) =>
      IfThenElse(
        g(t.attributes),
        t.condition.mapAttributes(f, g),
        t.thenBranch.mapAttributes(f, g),
        t.elseBranch.mapAttributes(f, g)
      )
    case t @ Lambda(_, _, _) => Lambda(g(t.attributes), t.argumentPattern.mapAttributes(g), t.body.mapAttributes(f, g))
    case t @ LetDefinition(_, _, _, _) =>
      LetDefinition(g(t.attributes), t.valueName, t.valueDefinition.mapAttributes(f, g), t.inValue.mapAttributes(f, g))
    case t @ LetRecursion(_, _, _) =>
      LetRecursion(
        g(t.attributes),
        t.valueDefinitions.map { case (name, definition) => (name, definition.mapAttributes(f, g)) },
        t.inValue.mapAttributes(f, g)
      )
    case t @ ListType(_, _)       => ListType(g(t.attributes), t.elements.map(_.mapAttributes(f, g)))
    case t @ Literal(_, _)        => Literal(g(t.attributes), t.literal)
    case t @ NativeApply(_, _, _) => NativeApply(g(t.attributes), t.function, t.arguments.map(_.mapAttributes(f, g)))
    case t @ PatternMatch(_, _, _) =>
      PatternMatch(
        g(t.attributes),
        t.branchOutOn.mapAttributes(f, g),
        t.cases.map { case (p, v) => (p.mapAttributes(g), v.mapAttributes(f, g)) }
      )
    case t @ Record(_, _)    => Record(g(t.attributes), t.fields.map { case (n, v) => (n, v.mapAttributes(f, g)) })
    case t @ Reference(_, _) => Reference(g(t.attributes), t.name)
    case t @ Tuple(_, _)     => Tuple(g(t.attributes), t.elements.map(item => item.mapAttributes(f, g)))
    case t @ UnitType(_)     => UnitType(g(t.attributes))
    case t @ UpdateRecord(_, _, _) =>
      UpdateRecord(
        g(t.attributes),
        t.valueToUpdate.mapAttributes(f, g),
        t.fieldsToUpdate.map { case (n, v) => (n, v.mapAttributes(f, g)) }
      )
    case t @ Variable(_, _) => Variable(g(t.attributes), t.name)
  }

  def collectVariables: Set[Name] = foldLeft(Set.empty[Name]) {
    case (acc, Variable(_, name)) => acc + name
    case (acc, _)                 => acc
  }

  def collectReferences: Set[FQName] = foldLeft(Set.empty[FQName]) {
    case (acc, Reference(_, name)) => acc + name
    case (acc, _)                  => acc
  }

  // def indexedMapValue[VB](initial: Int)(f: (Int, VA) => VB): (Value[TA, VB], Int) = ???
  def rewrite[TB >: TA, VB >: VA](pf: PartialFunction[Value[Caps, TB, VB], Value[Caps, TB, VB]]): Value[Caps, TB, VB] =
    transform[TB, VB](v => pf.lift(v).getOrElse(v))

  def toRawValue: RawValue = mapAttributes(_ => ZEnvironmentSubset.empty, _ => ZEnvironmentSubset.empty)

  def transform[TB >: TA, VB >: VA](f: Value[Caps, TB, VB] => Value[Caps, TB, VB]): Value[Caps, TB, VB] = fold[Value[Caps, TB, VB]](
    applyCase = (attributes, function, arguments) => f(Apply(attributes, function, arguments)),
    constructorCase = (attributes, name) => f(Constructor(attributes, name)),
    destructureCase =
      (attributes, pattern, valueToDestruct, inValue) => f(Destructure(attributes, pattern, valueToDestruct, inValue)),
    fieldCase = (attributes, target, name) => f(Field(attributes, target, name)),
    fieldFunctionCase = (attributes, name) => f(FieldFunction(attributes, name)),
    ifThenElseCase =
      (attributes, condition, thenBranch, elseBranch) => f(IfThenElse(attributes, condition, thenBranch, elseBranch)),
    lambdaCase = (attributes, argumentPattern, body) => f(Lambda(attributes, argumentPattern, body)),
    letDefinitionCase = (attributes, valueName, valueDefinition, inValue) =>
      f(LetDefinition(attributes, valueName, valueDefinition.toDefinition, inValue)),
    letRecursionCase = (attributes, valueDefinitions, inValue) =>
      f(LetRecursion(attributes, valueDefinitions.map { case (n, d) => (n, d.toDefinition) }, inValue)),
    listCase = (attributes, elements) => f(ListType(attributes, elements)),
    literalCase = (attributes, literal) => f(Literal(attributes, literal)),
    nativeApplyCase = (attributes, function, arguments) => f(NativeApply(attributes, function, arguments)),
    patternMatchCase = (attributes, branchOutOn, cases) => f(PatternMatch(attributes, branchOutOn, cases)),
    recordCase = (attributes, fields) => f(Record(attributes, fields)),
    referenceCase = (attributes, name) => f(Reference(attributes, name)),
    tupleCase = (attributes, elements) => f(Tuple(attributes, elements)),
    unitCase = (attributes) => f(UnitType(attributes)),
    updateRecordCase =
      (attributes, valueToUpdate, fieldsToUpdate) => f(UpdateRecord(attributes, valueToUpdate, fieldsToUpdate)),
    variableCase = (attributes, name) => f(Variable(attributes, name))
  )

  def fold[Z](
      applyCase: (ZEnvironmentSubset[Caps, VA], Z, Chunk[Z]) => Z,
      constructorCase: (ZEnvironmentSubset[Caps, VA], FQName) => Z,
      destructureCase: (ZEnvironmentSubset[Caps, VA], Pattern[Caps, VA], Z, Z) => Z,
      fieldCase: (ZEnvironmentSubset[Caps, VA], Z, Name) => Z,
      fieldFunctionCase: (ZEnvironmentSubset[Caps, VA], Name) => Z,
      ifThenElseCase: (ZEnvironmentSubset[Caps, VA], Z, Z, Z) => Z,
      lambdaCase: (ZEnvironmentSubset[Caps, VA], Pattern[Caps, VA], Z) => Z,
      letDefinitionCase: (ZEnvironmentSubset[Caps, VA], Name, Definition.Case[Caps, TA, VA, Z], Z) => Z,
      letRecursionCase: (ZEnvironmentSubset[Caps, VA], Map[Name, Definition.Case[Caps, TA, VA, Z]], Z) => Z,
      listCase: (ZEnvironmentSubset[Caps, VA], Chunk[Z]) => Z,
      literalCase: (ZEnvironmentSubset[Caps, VA], Lit[_]) => Z,
      nativeApplyCase: (ZEnvironmentSubset[Caps, VA], NativeFunction, Chunk[Z]) => Z,
      patternMatchCase: (ZEnvironmentSubset[Caps, VA], Z, Chunk[(Pattern[Caps, VA], Z)]) => Z,
      recordCase: (ZEnvironmentSubset[Caps, VA], Chunk[(Name, Z)]) => Z,
      referenceCase: (ZEnvironmentSubset[Caps, VA], FQName) => Z,
      tupleCase: (ZEnvironmentSubset[Caps, VA], Chunk[Z]) => Z,
      unitCase: ZEnvironmentSubset[Caps, VA] => Z,
      updateRecordCase: (ZEnvironmentSubset[Caps, VA], Z, Chunk[(Name, Z)]) => Z,
      variableCase: (ZEnvironmentSubset[Caps, VA], Name) => Z
  ): Z = self match {
    case Apply(attributes, function, arguments) =>
      applyCase(
        attributes,
        function.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        arguments.map(
          _.fold(
            applyCase,
            constructorCase,
            destructureCase,
            fieldCase,
            fieldFunctionCase,
            ifThenElseCase,
            lambdaCase,
            letDefinitionCase,
            letRecursionCase,
            listCase,
            literalCase,
            nativeApplyCase,
            patternMatchCase,
            recordCase,
            referenceCase,
            tupleCase,
            unitCase,
            updateRecordCase,
            variableCase
          )
        )
      )
    case Constructor(attributes, name) => constructorCase(attributes, name)
    case Destructure(attributes, pattern, valueToDestruct, inValue) =>
      destructureCase(
        attributes,
        pattern,
        valueToDestruct.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        inValue.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case Field(attributes, target, name) =>
      fieldCase(
        attributes,
        target.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        name
      )
    case FieldFunction(attributes, name) => fieldFunctionCase(attributes, name)
    case IfThenElse(attributes, condition, thenBranch, elseBranch) =>
      ifThenElseCase(
        attributes,
        condition.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        thenBranch.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        elseBranch.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case Lambda(attributes, argumentPattern, body) =>
      lambdaCase(
        attributes,
        argumentPattern,
        body.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case LetDefinition(attributes, valueName, valueDefinition, inValue) =>
      letDefinitionCase(
        attributes,
        valueName,
        valueDefinition.toCase.map(
          _.fold(
            applyCase,
            constructorCase,
            destructureCase,
            fieldCase,
            fieldFunctionCase,
            ifThenElseCase,
            lambdaCase,
            letDefinitionCase,
            letRecursionCase,
            listCase,
            literalCase,
            nativeApplyCase,
            patternMatchCase,
            recordCase,
            referenceCase,
            tupleCase,
            unitCase,
            updateRecordCase,
            variableCase
          )
        ),
        inValue.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case LetRecursion(attributes, valueDefinitions, inValue) =>
      letRecursionCase(
        attributes,
        valueDefinitions.map { case (n, d) =>
          (
            n,
            d.toCase.map(
              _.fold(
                applyCase,
                constructorCase,
                destructureCase,
                fieldCase,
                fieldFunctionCase,
                ifThenElseCase,
                lambdaCase,
                letDefinitionCase,
                letRecursionCase,
                listCase,
                literalCase,
                nativeApplyCase,
                patternMatchCase,
                recordCase,
                referenceCase,
                tupleCase,
                unitCase,
                updateRecordCase,
                variableCase
              )
            )
          )
        },
        inValue.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        )
      )
    case ListType(attributes, elements) =>
      listCase(
        attributes,
        elements.map(
          _.fold(
            applyCase,
            constructorCase,
            destructureCase,
            fieldCase,
            fieldFunctionCase,
            ifThenElseCase,
            lambdaCase,
            letDefinitionCase,
            letRecursionCase,
            listCase,
            literalCase,
            nativeApplyCase,
            patternMatchCase,
            recordCase,
            referenceCase,
            tupleCase,
            unitCase,
            updateRecordCase,
            variableCase
          )
        )
      )
    case Literal(attributes, literal) => literalCase(attributes, literal)
    case NativeApply(attributes, function, arguments) =>
      nativeApplyCase(
        attributes,
        function,
        arguments.map(
          _.fold(
            applyCase,
            constructorCase,
            destructureCase,
            fieldCase,
            fieldFunctionCase,
            ifThenElseCase,
            lambdaCase,
            letDefinitionCase,
            letRecursionCase,
            listCase,
            literalCase,
            nativeApplyCase,
            patternMatchCase,
            recordCase,
            referenceCase,
            tupleCase,
            unitCase,
            updateRecordCase,
            variableCase
          )
        )
      )
    case PatternMatch(attributes, branchOutOn, cases) =>
      patternMatchCase(
        attributes,
        branchOutOn.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        cases.map { case (p, v) =>
          (
            p,
            v.fold(
              applyCase,
              constructorCase,
              destructureCase,
              fieldCase,
              fieldFunctionCase,
              ifThenElseCase,
              lambdaCase,
              letDefinitionCase,
              letRecursionCase,
              listCase,
              literalCase,
              nativeApplyCase,
              patternMatchCase,
              recordCase,
              referenceCase,
              tupleCase,
              unitCase,
              updateRecordCase,
              variableCase
            )
          )
        }
      )
    case Record(attributes, fields) =>
      recordCase(
        attributes,
        fields.map { case (n, v) =>
          (
            n,
            v.fold(
              applyCase,
              constructorCase,
              destructureCase,
              fieldCase,
              fieldFunctionCase,
              ifThenElseCase,
              lambdaCase,
              letDefinitionCase,
              letRecursionCase,
              listCase,
              literalCase,
              nativeApplyCase,
              patternMatchCase,
              recordCase,
              referenceCase,
              tupleCase,
              unitCase,
              updateRecordCase,
              variableCase
            )
          )
        }
      )
    case Reference(attributes, name) => referenceCase(attributes, name)
    case Tuple(attributes, elements) =>
      tupleCase(
        attributes,
        elements.map(
          _.fold(
            applyCase,
            constructorCase,
            destructureCase,
            fieldCase,
            fieldFunctionCase,
            ifThenElseCase,
            lambdaCase,
            letDefinitionCase,
            letRecursionCase,
            listCase,
            literalCase,
            nativeApplyCase,
            patternMatchCase,
            recordCase,
            referenceCase,
            tupleCase,
            unitCase,
            updateRecordCase,
            variableCase
          )
        )
      )
    case UnitType(attributes) => unitCase(attributes)
    case UpdateRecord(attributes, valueToUpdate, fieldsToUpdate) =>
      updateRecordCase(
        attributes,
        valueToUpdate.fold(
          applyCase,
          constructorCase,
          destructureCase,
          fieldCase,
          fieldFunctionCase,
          ifThenElseCase,
          lambdaCase,
          letDefinitionCase,
          letRecursionCase,
          listCase,
          literalCase,
          nativeApplyCase,
          patternMatchCase,
          recordCase,
          referenceCase,
          tupleCase,
          unitCase,
          updateRecordCase,
          variableCase
        ),
        fieldsToUpdate.map { case (n, v) =>
          (
            n,
            v.fold(
              applyCase,
              constructorCase,
              destructureCase,
              fieldCase,
              fieldFunctionCase,
              ifThenElseCase,
              lambdaCase,
              letDefinitionCase,
              letRecursionCase,
              listCase,
              literalCase,
              nativeApplyCase,
              patternMatchCase,
              recordCase,
              referenceCase,
              tupleCase,
              unitCase,
              updateRecordCase,
              variableCase
            )
          )
        }
      )
    case Variable(attributes, name) => variableCase(attributes, name)
  }

  def foldLeft[Z](initial: Z)(f: (Z, Value[Caps, TA, VA]) => Z): Z = {
    @tailrec
    def loop(stack: List[Value[Caps, TA, VA]], acc: Z): Z =
      stack match {
        case Nil                                   => acc
        case (t @ Apply(_, _, _)) :: tail          => loop(t.function :: t.arguments.toList ::: tail, f(acc, t))
        case (t @ Constructor(_, _)) :: tail       => loop(tail, f(acc, t))
        case (t @ Destructure(_, _, _, _)) :: tail => loop(t.valueToDestruct :: t.inValue :: tail, f(acc, t))
        case (t @ Field(_, _, _)) :: tail          => loop(t.target :: tail, f(acc, t))
        case (t @ FieldFunction(_, _)) :: tail     => loop(tail, f(acc, t))
        case (t @ IfThenElse(_, _, _, _)) :: tail =>
          loop(t.condition :: t.thenBranch :: t.elseBranch :: tail, f(acc, t))
        case (t @ Lambda(_, _, _)) :: tail           => loop(t.body :: tail, f(acc, t))
        case (t @ LetDefinition(_, _, _, _)) :: tail => loop(t.valueDefinition.body :: t.inValue :: tail, f(acc, t))
        case (t @ LetRecursion(_, _, _)) :: tail =>
          loop(t.valueDefinitions.map(_._2.body).toList ::: t.inValue :: tail, f(acc, t))
        case (t @ ListType(_, _)) :: tail        => loop(t.elements.toList ::: tail, f(acc, t))
        case (t @ Literal(_, _)) :: tail         => loop(tail, f(acc, t))
        case (t @ NativeApply(_, _, _)) :: tail  => loop(t.arguments.toList ::: tail, f(acc, t))
        case (t @ PatternMatch(_, _, _)) :: tail => loop(t.branchOutOn :: t.cases.map(_._2).toList ::: tail, f(acc, t))
        case (t @ Record(_, _)) :: tail          => loop(t.fields.map(_._2).toList ::: tail, f(acc, t))
        case (t @ Reference(_, _)) :: tail       => loop(tail, f(acc, t))
        case (t @ Tuple(_, _)) :: tail           => loop(t.elements.toList ::: tail, f(acc, t))
        case (t @ UnitType(_)) :: tail           => loop(tail, f(acc, t))
        case (t @ UpdateRecord(_, _, _)) :: tail =>
          loop(t.valueToUpdate :: t.fieldsToUpdate.map(_._2).toList ::: tail, f(acc, t))
        case (t @ Variable(_, _)) :: tail => loop(tail, f(acc, t))
      }

    loop(List(self), initial)
  }

}

object Value {

  final case class Apply[+Caps[_], +TA, +VA](attributes: ZEnvironmentSubset[Caps, VA], function: Value[Caps, TA, VA], arguments: Chunk[Value[Caps, TA, VA]])
      extends Value[Caps, TA, VA]

  object Apply {
    type Raw[Caps[_]] = Apply[Caps, Any, Any]

    object Raw {
      def apply(function: RawValue, arguments: Chunk[RawValue]): Raw =
        Apply(ZEnvironmentSubset.empty, function, arguments)

      def apply(function: RawValue, arguments: RawValue*): Raw =
        Apply(ZEnvironmentSubset.empty, function, Chunk.fromArray(arguments.toArray))
    }

    type Typed = Apply[Any, UType]
    object Typed {
      def apply(function: TypedValue, arguments: Chunk[TypedValue]): Typed =
        Apply(function.attributes, function, arguments)

      def apply(function: TypedValue, arguments: TypedValue*): Typed =
        Apply(function.attributes, function, Chunk.fromArray(arguments.toArray))
    }
  }

  final case class Constructor[+Caps[_], +VA](attributes: ZEnvironmentSubset[Caps, VA], name: FQName) extends Value[Caps, Nothing, VA]
  object Constructor {
    type Raw = Constructor[Any]
    object Raw {
      def apply(name: FQName): Raw = Constructor(ZEnvironmentSubset.empty, name)
    }
    type Typed = Constructor[UType]
    object Typed {
      def apply(name: FQName)(ascribedType: UType): Typed   = Constructor(ZEnvironmentSubset(ascribedType), name)
      def apply(fqName: String)(ascribedType: UType): Typed = Constructor(ZEnvironmentSubset(ascribedType), FQName.fromString(fqName))
    }
  }

  final case class Destructure[+Caps[_], +TA, +VA](
      attributes: ZEnvironmentSubset[AnyType, VA],
      pattern: Pattern[Caps, VA],
      valueToDestruct: Value[Caps, TA, VA],
      inValue: Value[Caps, TA, VA]
  ) extends Value[Caps, TA, VA]

  object Destructure {
    type Raw = Destructure[Any, Any]
    def apply[Caps[_]](pattern: Pattern[Caps, Any], valueToDestruct: RawValue, inValue: RawValue): Raw =
      Destructure(ZEnvironmentSubset.empty, pattern, valueToDestruct, inValue)
  }

  final case class Field[+Caps[_], +TA, +VA](attributes: ZEnvironmentSubset[Caps, VA], target: Value[Caps, TA, VA], name: Name) extends Value[Caps, TA, VA]

  object Field {
    type Raw = Field[Any, Any]
    object Raw {
      def apply(target: RawValue, name: Name): Raw = Field(ZEnvironmentSubset.empty, target, name)
    }
    type Typed = Field[Any, UType]
    object Typed {
      def apply(target: TypedValue, name: Name)(ascribedType: UType): Typed = Field(ZEnvironmentSubset[AnyType, UType](ascribedType), target, name)
      def apply(fieldType: UType, target: TypedValue, name: Name): Typed    = Field(ZEnvironmentSubset(fieldType), target, name)
      def apply(target: TypedValue, name: String)(ascribedType: UType): Typed =
        Field(ZEnvironmentSubset(ascribedType), target, Name.fromString(name))
      def apply(fieldType: UType, target: TypedValue, name: String): Typed =
        Field(ZEnvironmentSubset(fieldType), target, Name.fromString(name))
    }
  }
  final case class FieldFunction[+Caps[_], +VA](attributes: ZEnvironmentSubset[Caps, VA], name: Name) extends Value[Caps, Nothing, VA]

  object FieldFunction {
    type Raw = FieldFunction[AnyType, Any]
    object Raw {
      def apply(name: Name): Raw = FieldFunction(ZEnvironmentSubset.empty, name)
    }
    type Typed[+Caps[_]] = FieldFunction[Caps, UType]
    object Typed {
      def apply(name: Name)(ascribedType: UType): Typed   = FieldFunction(ZEnvironmentSubset(ascribedType), name)
      def apply(name: String)(ascribedType: UType): Typed = FieldFunction(ZEnvironmentSubset(ascribedType), Name.fromString(name))
    }
  }

  final case class IfThenElse[+Caps[_], +TA, +VA](
      attributes: ZEnvironmentSubset[Caps, VA],
      condition: Value[Caps, TA, VA],
      thenBranch: Value[Caps, TA, VA],
      elseBranch: Value[Caps, TA, VA]
  ) extends Value[Caps, TA, VA]

  object IfThenElse {
    type Raw = IfThenElse[Any, Any]
    def apply(condition: RawValue, thenBranch: RawValue, elseBranch: RawValue): Raw =
      IfThenElse(ZEnvironmentSubset.empty, condition, thenBranch, elseBranch)
  }

  final case class Lambda[+Caps[_], +TA, +VA](attributes: ZEnvironmentSubset[Caps, VA], argumentPattern: Pattern[Caps, VA], body: Value[Caps, TA, VA])
      extends Value[Caps, TA, VA]

  object Lambda {
    type Raw = Lambda[AnyType, Any, Any]
    object Raw {
      def apply(argumentPattern: Pattern[Any], body: RawValue): Raw =
        Lambda(ZEnvironmentSubset.empty, argumentPattern, body)
    }
    type Typed[+Caps[_]] = Lambda[Caps, Any, UType]
    object Typed {
      def apply[Caps[_]](argumentPattern: Pattern[Caps, UType], body: TypedValue[Caps]): Typed[Caps] =
        Lambda(ZEnvironmentSubset[Caps, UType](body.attributes.get._1), argumentPattern, body)
    }
  }

  final case class LetDefinition[+Caps[_], +TA, +VA](
      attributes: ZEnvironmentSubset[Caps, VA],
      valueName: Name,
      valueDefinition: Definition[Caps, TA, VA],
      inValue: Value[Caps, TA, VA]
  ) extends Value[Caps, TA, VA]

  object LetDefinition {
    type Raw = LetDefinition[AnyType, Any, Any]
    def apply(valueName: Name, valueDefinition: Definition[Any, Any], inValue: RawValue): Raw =
      LetDefinition(ZEnvironmentSubset.empty, valueName, valueDefinition, inValue)
  }

  final case class LetRecursion[+Caps[_], +TA, +VA](
      attributes: ZEnvironmentSubset[Caps, VA],
      valueDefinitions: Map[Name, Definition[Caps, TA, VA]],
      inValue: Value[Caps, TA, VA]
  ) extends Value[Caps, TA, VA]

  object LetRecursion {
    type Raw = LetRecursion[AnyType, Any, Any]
    object Raw {
      def apply(valueDefinitions: Map[Name, Definition[Any, Any]], inValue: RawValue): Raw =
        LetRecursion(ZEnvironmentSubset.empty, valueDefinitions, inValue)
    }
    type Typed[+Caps[_]] = LetRecursion[Caps, Any, UType]
    object Typed {
      def apply(valueDefinitions: Map[Name, Definition[Any, UType]], inValue: TypedValue): Typed =
        LetRecursion(ZEnvironmentSubset[AnyType, UType](inValue.attributes.get._1), valueDefinitions, inValue)
      def apply(defs: (String, Definition[Any, UType])*)(inValue: TypedValue): Typed =
        LetRecursion(ZEnvironmentSubset(inValue.attributes.get._1), defs.map { case (n, v) => (Name.fromString(n), v) }.toMap, inValue)
    }
  }

  final case class List[+Caps[_], +TA, +VA](attributes: ZEnvironmentSubset[Caps, VA], elements: Chunk[Value[Caps, TA, VA]]) extends Value[Caps, TA, VA]

  object List {
    // def nonEmpty[TA, VA](first: Value[TA, VA], rest: Value[TA, VA]*): List[TA, VA] =
    //   List(first.attributes.get, first +: Chunk.fromIterable(rest))
    type Raw = List[Any, Any]
    object Raw {
      def apply(elements: RawValue*): Raw       = List(ZEnvironmentSubset.empty, Chunk.fromArray(elements.toArray))
      def apply(elements: Chunk[RawValue]): Raw = List(ZEnvironmentSubset.empty, elements)
    }

    type Typed = List[Any, UType]
    object Typed {
      def empty(ascribedType: UType): Typed                              = List(ZEnvironmentSubset(ascribedType), Chunk.empty)
      def apply[Caps[_]](elements: Chunk[TypedValue[Caps]])(ascribedType: UType): Typed = List(ZEnvironmentSubset(ascribedType), elements)
      def apply[Caps](elements: TypedValue[Caps]*)(ascribedType: UType): Typed = List(ZEnvironmentSubset(ascribedType), Chunk.fromIterable(elements))
    }
  }

  final case class Literal[+Caps[_], +VA, +A](attributes: ZEnvironmentSubset[Caps, VA], literal: Lit[A]) extends Value[Caps, Nothing, VA]

  object Literal {
    type Raw[+A] = Literal[Any, A]
    object Raw {
      def apply[A](literal: Lit[A]): Raw[A] = Literal(ZEnvironmentSubset.empty, literal)
    }

    type Typed[+A] = Literal[UType, A]
    object Typed {
      def apply[A](literal: Lit[A])(ascribedType: UType): Typed[A] = Literal(ZEnvironmentSubset(ascribedType), literal)
      def apply[A](literal: Lit[A])(implicit ev: InferredTypeOf[Lit[A]]): Typed[A] =
        Literal(ZEnvironmentSubset(ev.inferredType(literal)), literal)
    }
  }

  final case class NativeApply[+Caps[_], +TA, +VA](attributes: ZEnvironmentSubset[AnyType, VA], function: NativeFunction, arguments: Chunk[Value[Caps, TA, VA]])
      extends Value[Caps, TA, VA]

  object NativeApply {
    type Raw = NativeApply[Any, Any]
    def apply(function: NativeFunction, arguments: Chunk[RawValue]): Raw =
      NativeApply(ZEnvironmentSubset.empty, function, arguments)
  }

  final case class PatternMatch[+Caps[_], +TA, +VA](
      attributes: ZEnvironmentSubset[Caps, VA],
      branchOutOn: Value[Caps, TA, VA],
      cases: Chunk[(Pattern[VA], Value[Caps, TA, VA])]
  ) extends Value[Caps, TA, VA]

  object PatternMatch {
    type Raw = PatternMatch[Any, Any]
    object Raw {
      def apply(branchOutOn: RawValue, cases: Chunk[(Pattern[Any], RawValue)]): Raw =
        PatternMatch(ZEnvironmentSubset.empty, branchOutOn, cases)
    }

    type Typed = PatternMatch[Any, UType]
    object Typed {
      def apply(branchOutOn: TypedValue, cases: Chunk[(Pattern[UType], TypedValue)]): Typed =
        PatternMatch(branchOutOn.attributes, branchOutOn, cases)

      def apply(branchOutOn: TypedValue, cases: (Pattern[UType], TypedValue)*): Typed =
        PatternMatch(branchOutOn.attributes, branchOutOn, Chunk.fromIterable(cases))
    }
  }

  final case class Record[+Caps[_], +TA, +VA](attributes: ZEnvironmentSubset[AnyType, VA], fields: Chunk[(Name, Value[Caps, TA, VA])]) extends Value[Caps, TA, VA]

  object Record {
    type Raw = Record[Any, Any]

    object Raw {
      def apply(fields: Chunk[(Name, RawValue)]): Raw = Record(ZEnvironmentSubset.empty, fields)
      def apply(fields: (String, RawValue)*): Raw = Record(
        attributes = ZEnvironmentSubset.empty,
        fields = Chunk.fromIterable(fields).map { case (n, v) => Name.fromString(n) -> v }
      )
    }

    type Typed = Record[Any, UType]
    object Typed {
      def apply(recordType: UType, fields: (String, TypedValue)*): Typed = Record(
        attributes = ZEnvironmentSubset(recordType),
        fields = Chunk.fromIterable(fields).map { case (n, v) => (Name.fromString(n), v) }
      )

      def apply(fields: (String, TypedValue)*): Typed = {
        val allFields  = Chunk.fromIterable(fields.map { case (n, v) => (Name.fromString(n), v) })
        val recordType = Type.record(allFields.map { case (n, v) => Type.field(n, ???) })
        Record(ZEnvironmentSubset(recordType), allFields)
      }
    }
  }

  final case class Reference[+VA](attributes: ZEnvironmentSubset[AnyType, VA], name: FQName) extends Value[Nothing, VA]

  object Reference {
    def apply(name: FQName): Raw = Reference(ZEnvironmentSubset.empty, name)

    type Raw = Reference[Any]
    object Raw {
      def apply(name: FQName): Raw = Reference(ZEnvironmentSubset.empty, name)
    }

    type Typed = Reference[UType]
    object Typed {
      def apply(fqName: String)(refType: UType): Typed = Reference(ZEnvironmentSubset(refType), FQName.fromString(fqName))
      def apply(name: FQName)(refType: UType): Typed   = Reference(ZEnvironmentSubset(refType), name)
    }
  }

  final case class Tuple[+Caps[_], +TA, +VA](attributes: ZEnvironmentSubset[Caps, VA], elements: Chunk[Value[Caps, TA, VA]]) extends Value[Caps, TA, VA]

  object Tuple {
    val empty: Raw = Tuple(ZEnvironmentSubset.empty, Chunk.empty)
    type Raw = Tuple[Any, Any]

    object Raw {
      def apply(elements: RawValue*): Raw       = Tuple(ZEnvironmentSubset.empty, Chunk(elements: _*))
      def apply(elements: Chunk[RawValue]): Raw = Tuple(ZEnvironmentSubset.empty, elements)
    }

    type Typed = Tuple[Any, UType]
    object Typed {
      def apply(elements: (RawValue, UType)*): Typed = {
        Tuple(
          ZEnvironmentSubset(Type.Tuple.Raw(elements.map(_._2): _*)),
          Chunk(elements: _*).map { case (v, t) => v :@ ZEnvironmentSubset(t) }
        )
      }
    }
  }

  final case class Unit[+Caps[_], +VA](attributes: ZEnvironmentSubset[Caps, VA]) extends Value[Nothing, VA]
  object Unit {
    type Raw = Unit[Any]
    def Raw: Raw = Unit(ZEnvironmentSubset.empty)

    type Typed = Unit[UType]
    object Typed {
      def apply: Typed = Value.Unit(ZEnvironmentSubset(Type.unit))
    }
  }

  final case class UpdateRecord[+Caps[_], +TA, +VA](
      attributes: ZEnvironmentSubset[Caps, VA],
      valueToUpdate: Value[Caps, TA, VA],
      fieldsToUpdate: Chunk[(Name, Value[Caps, TA, VA])]
  ) extends Value[Caps, TA, VA]

  object UpdateRecord {
    type Raw = UpdateRecord[Any, Any]
    object Raw {
      def apply(valueToUpdate: RawValue, fieldsToUpdate: Chunk[(Name, RawValue)]): Raw =
        UpdateRecord(ZEnvironmentSubset.empty, valueToUpdate, fieldsToUpdate)
    }

    type Typed = UpdateRecord[Any, UType]
    object Typed {
      def apply(valueToUpdate: TypedValue, fieldsToUpdate: Chunk[(Name, TypedValue)]): Typed = {
        UpdateRecord(
          valueToUpdate.attributes,
          valueToUpdate,
          fieldsToUpdate
        )
      }

      def apply(valueToUpdate: TypedValue, fieldsToUpdate: (String, TypedValue)*): Typed =
        UpdateRecord(
          valueToUpdate.attributes,
          valueToUpdate,
          Chunk.fromIterable(fieldsToUpdate).map { case (n, v) => (Name.fromString(n), v) }
        )
    }
  }

  final case class Variable[+Caps[_], +VA](attributes: ZEnvironmentSubset[Caps, VA], name: Name) extends Value[Caps, Nothing, VA]
  object Variable {
    type Raw = Variable[Any]
    object Raw {
      def apply(name: Name): Raw   = Variable(ZEnvironmentSubset.empty, name)
      def apply(name: String): Raw = Variable(ZEnvironmentSubset.empty, Name.fromString(name))
    }
    type Typed = Variable[UType]
    object Typed {
      def apply(name: Name)(variableType: UType): Typed   = Variable(ZEnvironmentSubset(variableType), name)
      def apply(name: String)(variableType: UType): Typed = Variable(ZEnvironmentSubset(variableType), Name.fromString(name))
    }
  }

  implicit class RawValueExtensions(val self: RawValue) extends AnyVal {

    /**
     * Ascribe the given type to this `RawValue` and all its children.
     * ===NOTE===
     * This is a recursive operation and all children of this `RawValue` will also be ascribed with the given value.
     */
    def :@[Caps](ascribedType: ZEnvironmentSubset[AnyType, UType]): TypedValue[Caps] = self.mapAttributes(identity, _ => ascribedType)

    /**
     * Ascribe the given type to this `RawValue` and all its children.
     * ===NOTE===
     * This is a recursive operation and all children of this `RawValue` will also be ascribed with the given value.
     */
    def @:[Caps[_]](ascribedType: ZEnvironmentSubset[AnyType, UType]): TypedValue[Caps] = self.mapAttributes(identity, _ => ascribedType)
  }

  implicit class TypedValueExtensions[Caps[_]](val self: TypedValue[Caps]) extends AnyVal {

    /**
     * Ascribe the given type to the value.
     * ===NOTE===
     * This is not a recursive operation on a `TypedValue` and it will only ascribe the type of this given value and not
     * its children.
     */
    def :@(ascribedType: ZEnvironmentSubset[AnyType, UType]): TypedValue = self match {
      case Apply(_, function, arguments) => Apply(ascribedType, function, arguments)
      case Constructor(_, name)          => Constructor(ascribedType, name)
      case Destructure(_, pattern, valueToDestruct, inValue) =>
        Destructure(ascribedType, pattern, valueToDestruct, inValue)
      case Field(_, target, name) => Field(ascribedType, target, name)
      case FieldFunction(_, name) => FieldFunction(ascribedType, name)
      case IfThenElse(_, condition, thenBranch, elseBranch) =>
        IfThenElse(ascribedType, condition, thenBranch, elseBranch)
      case Lambda(_, argumentPattern, body) => Lambda(ascribedType, argumentPattern, body)
      case LetDefinition(_, valueName, valueDefinition, inValue) =>
        LetDefinition(ascribedType, valueName, valueDefinition, inValue)
      case LetRecursion(_, valueDefinitions, inValue)     => LetRecursion(ascribedType, valueDefinitions, inValue)
      case List(_, elements)                              => List(ascribedType, elements)
      case Literal(_, literal)                            => Literal(ascribedType, literal)
      case NativeApply(_, function, arguments)            => NativeApply(ascribedType, function, arguments)
      case PatternMatch(_, branchOutOn, cases)            => PatternMatch(ascribedType, branchOutOn, cases)
      case Record(_, fields)                              => Record(ascribedType, fields)
      case Reference(_, name)                             => Reference(ascribedType, name)
      case Tuple(_, elements)                             => Tuple(ascribedType, elements)
      case Unit(_)                                        => Unit(ascribedType)
      case UpdateRecord(_, valueToUpdate, fieldsToUpdate) => UpdateRecord(ascribedType, valueToUpdate, fieldsToUpdate)
      case Variable(_, name)                              => Variable(ascribedType, name)
    }
  }
}
