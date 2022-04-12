package zio.morphir.ir.value.recursive

import zio.morphir.ir.Type.UType
import zio.morphir.ir.value.Pattern
import zio.morphir.ir.{FQName, Literal => Lit, Name, Path}
import zio.prelude._
import zio.prelude.fx.ZPure
import zio.{Chunk, ZIO}

import scala.annotation.tailrec
final case class Value[+TA, +VA](caseValue: ValueCase[TA, VA, Value[TA, VA]]) { self =>
  import ValueCase._
  def attributes: VA = caseValue.attributes

  def collectReferences: Set[FQName] = fold[Set[FQName]] {
    case c @ ApplyCase(_, _, _)            => c.function ++ c.argument
    case c @ DestructureCase(_, _, _, _)   => c.valueToDestruct ++ c.inValue
    case c @ FieldCase(_, _, _)            => c.target
    case _ @FieldFunctionCase(_, _)        => Set.empty
    case c @ IfThenElseCase(_, _, _, _)    => c.condition ++ c.thenBranch ++ c.elseBranch
    case c @ LambdaCase(_, _, _)           => c.body
    case c @ LetDefinitionCase(_, _, _, _) => c.valueDefinition.body ++ c.inValue
    case c @ LetRecursionCase(_, _, _) =>
      c.valueDefinitions.foldLeft(Set.empty[FQName])((acc, kv) => acc ++ kv._2.body)
    case c @ ListCase(_, _)            => c.elements.flatten.toSet
    case _ @LiteralCase(_, _)          => Set.empty
    case c @ PatternMatchCase(_, _, _) => c.cases.flatMap(_._2).toSet ++ c.branchOutOn
    case c @ RecordCase(_, _)          => c.fields.flatMap(_._2).toSet
    case c @ ReferenceCase(_, _)       => Set(c.name)
    case c @ TupleCase(_, _)           => c.elements.flatten.toSet
    case _ @UnitCase(_)                => Set.empty
    case c @ UpdateRecordCase(_, _, _) => c.fieldsToUpdate.flatMap(_._2).toSet ++ c.valueToUpdate
    case _ @VariableCase(_, _)         => Set.empty
    case _                             => Set.empty
  }
  def collectVariables: Set[Name] = fold[Set[Name]] {
    case c @ ApplyCase(_, _, _)            => c.function ++ c.argument
    case c @ DestructureCase(_, _, _, _)   => c.valueToDestruct ++ c.inValue
    case c @ FieldCase(_, _, _)            => c.target
    case c @ FieldFunctionCase(_, _)       => Set(c.name)
    case c @ IfThenElseCase(_, _, _, _)    => c.condition ++ c.thenBranch ++ c.elseBranch
    case c @ LambdaCase(_, _, _)           => c.body
    case c @ LetDefinitionCase(_, _, _, _) => c.valueDefinition.body ++ c.inValue + c.valueName
    case c @ LetRecursionCase(_, _, _) =>
      c.valueDefinitions.foldLeft(Set.empty[Name])((acc, kv) => acc ++ kv._2.body + kv._1)
    case c @ ListCase(_, _)            => c.elements.flatten.toSet
    case _ @LiteralCase(_, _)          => Set.empty
    case c @ PatternMatchCase(_, _, _) => c.cases.flatMap(_._2).toSet ++ c.branchOutOn
    case c @ RecordCase(_, _)          => c.fields.flatMap(_._2).toSet
    case c @ TupleCase(_, _)           => c.elements.flatten.toSet
    case _ @UnitCase(_)                => Set.empty
    case c @ UpdateRecordCase(_, _, _) => c.fieldsToUpdate.flatMap(_._2).toSet ++ c.valueToUpdate
    case c @ VariableCase(_, _)        => Set(c.name)
    case _                             => Set.empty
  }

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

  def foldDown[Z](z: Z)(f: (Z, Value[TA, VA]) => Z): Z =
    caseValue.foldLeft(f(z, self))((z, recursive) => recursive.foldDown(z)(f))

  def foldDownSome[Z](z: Z)(pf: PartialFunction[(Z, Value[TA, VA]), Z]): Z =
    foldDown(z)((z, recursive) => pf.lift(z -> recursive).getOrElse(z))

  def foldLeft[Z](initial: Z)(f: (Z, Value[TA, VA]) => Z): Z = {
    @tailrec
    def loop(stack: List[Value[TA, VA]], acc: Z): Z =
      stack match {
        case Nil                                      => acc
        case Value(v @ ApplyCase(_, _, _)) :: tail    => loop(v.function :: v.argument :: tail, f(acc, Value(v)))
        case Value(v @ ConstructorCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
        case Value(v @ DestructureCase(_, _, _, _)) :: tail =>
          loop(v.valueToDestruct :: v.inValue :: tail, f(acc, Value(v)))
        case Value(v @ FieldCase(_, _, _)) :: tail      => loop(v.target :: tail, f(acc, Value(v)))
        case Value(v @ FieldFunctionCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
        case Value(v @ IfThenElseCase(_, _, _, _)) :: tail =>
          loop(v.condition :: v.thenBranch :: v.elseBranch :: tail, f(acc, Value(v)))
        case Value(v @ LambdaCase(_, _, _)) :: tail => loop(v.body :: tail, f(acc, Value(v)))
        case Value(v @ LetDefinitionCase(_, _, _, _)) :: tail =>
          loop(v.valueDefinition.body :: v.inValue :: tail, f(acc, Value(v)))
        case Value(v @ LetRecursionCase(_, _, _)) :: tail =>
          loop(v.valueDefinitions.map(_._2.body).toList ::: v.inValue :: tail, f(acc, Value(v)))
        case Value(v @ ListCase(_, _)) :: tail    => loop(v.elements.toList ::: tail, f(acc, Value(v)))
        case Value(v @ LiteralCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
        case Value(v @ PatternMatchCase(_, _, _)) :: tail =>
          loop(v.branchOutOn :: v.cases.map(_._2).toList ::: tail, f(acc, Value(v)))
        case Value(v @ RecordCase(_, _)) :: tail    => loop(v.fields.map(_._2).toList ::: tail, f(acc, Value(v)))
        case Value(v @ ReferenceCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
        case Value(v @ TupleCase(_, _)) :: tail     => loop(v.elements.toList ::: tail, f(acc, Value(v)))
        case Value(v @ UnitCase(_)) :: tail         => loop(tail, f(acc, Value(v)))
        case Value(v @ UpdateRecordCase(_, _, _)) :: tail =>
          loop(v.valueToUpdate :: v.fieldsToUpdate.map(_._2).toList ::: tail, f(acc, Value(v)))
        case Value(v @ VariableCase(_, _)) :: tail => loop(tail, f(acc, Value(v)))
      }

    loop(List(self), initial)
  }

  def foldM[F[+_]: AssociativeFlatten: Covariant: IdentityBoth, Z](f: ValueCase[TA, VA, Z] => F[Z]): F[Z] =
    fold[F[Z]](_.flip.flatMap(f))

  def foldPure[W, S, R, E, Z](f: ValueCase[TA, VA, Z] => ZPure[W, S, S, R, E, Z]): ZPure[W, S, S, R, E, Z] =
    foldM(f)

  def foldRecursive[Z](f: ValueCase[TA, VA, (Value[TA, VA], Z)] => Z): Z =
    f(caseValue.map(recursive => recursive -> recursive.foldRecursive(f)))

  def foldUp[Z](z: Z)(f: (Z, Value[TA, VA]) => Z): Z =
    f(caseValue.foldLeft(z)((z, recursive) => recursive.foldUp(z)(f)), self)

  def foldZIO[R, E, Z](f: ValueCase[TA, VA, Z] => ZIO[R, E, Z]): ZIO[R, E, Z] = foldM(f)

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

  def toRawValue: RawValue = mapAttributes(_ => (), _ => ())

  override def toString: String =
    foldRecursive[String] {
      case ApplyCase(attributes, function, argument)                          => ???
      case ConstructorCase(_, name)                                           => name.toReferenceName
      case DestructureCase(attributes, pattern, valueToDestruct, inValue)     => ???
      case FieldCase(attributes, target, name)                                => ???
      case FieldFunctionCase(_, name)                                         => s".${name.toCamelCase}"
      case IfThenElseCase(attributes, condition, thenBranch, elseBranch)      => ???
      case LambdaCase(attributes, argumentPattern, body)                      => ???
      case LetDefinitionCase(attributes, valueName, valueDefinition, inValue) => ???
      case LetRecursionCase(attributes, valueDefinitions, inValue)            => ???
      case ListCase(attributes, elements)                                     => ???
      case LiteralCase(_, literal)                                            => literal.toString
      case PatternMatchCase(attributes, branchOutOn, cases)                   => ???
      case RecordCase(attributes, fields)                                     => ???
      case ReferenceCase(_, name) =>
        Seq(
          Path.toString(Name.toTitleCase, ".", name.packagePath.toPath),
          Path.toString(Name.toTitleCase, ".", name.modulePath.toPath),
          name.localName.toCamelCase
        ).mkString(".")
      case TupleCase(attributes, elements) =>
        elements.map(_._2).mkString("(", ", ", ")")
      case UnitCase(attributes)                                        => "()"
      case UpdateRecordCase(attributes, valueToUpdate, fieldsToUpdate) => ???
      case VariableCase(_, name)                                       => name.toCamelCase
    }.toString()
}

object Value extends ValueConstructors {
  import ValueCase._

  type RawValue = Value[Any, Any]
  val RawValue: Value.type = Value

  type TypedValue = Value[Any, UType]
  val TypedValue: Value.type = Value

  def apply[TA, VA](attributes: VA, function: Value[TA, VA], argument: Value[TA, VA]): Value[TA, VA] =
    Value(ApplyCase(attributes, function, argument))

  object Constructor {
    def apply[A](attributes: A, name: String): Value[Nothing, A] = Value(
      ConstructorCase(attributes, FQName.fromString(name))
    )

    def apply[A](attributes: A, name: FQName): Value[Nothing, A] = Value(ConstructorCase(attributes, name))

    def unapply[A](value: Value[Nothing, A]): Option[(A, FQName)] = value.caseValue match {
      case ConstructorCase(attributes, name) => Some((attributes, name))
      case _                                 => None
    }

    object Raw {
      @inline def apply(name: String): Value[Nothing, Any] = Constructor((), name)
      @inline def apply(name: FQName): Value[Nothing, Any] = Constructor((), name)
      def unapply(value: Value[Nothing, Any]): Option[FQName] = value.caseValue match {
        case ConstructorCase(_, name) => Some(name)
        case _                        => None
      }
    }
  }

  object FieldFunction {
    def apply[VA](attributes: VA, name: String): Value[Nothing, VA] = Value(
      FieldFunctionCase(attributes, Name.fromString(name))
    )
    def apply[VA](attributes: VA, name: Name): Value[Nothing, VA] = Value(FieldFunctionCase(attributes, name))

    def unapply[VA](value: Value[Nothing, VA]): Option[(VA, Name)] = value.caseValue match {
      case FieldFunctionCase(attributes, name) => Some((attributes, name))
      case _                                   => None
    }

    object Raw {
      @inline def apply(name: String): RawValue = FieldFunction((), name)
      @inline def apply(name: Name): RawValue   = FieldFunction((), name)

      def unapply(value: RawValue): Option[Name] = value.caseValue match {
        case FieldFunctionCase(_, name) => Some(name)
        case _                          => None
      }
    }
  }

  object Lambda {
    def apply[TA, VA](attributes: VA, argumentPattern: Pattern[VA], body: Value[TA, VA]): Value[TA, VA] =
      Value(LambdaCase(attributes, argumentPattern, body))
  }

  object Literal {
    def apply[VA, A](attributes: VA, literal: Lit[A]): Value[Nothing, VA] =
      Value(LiteralCase(attributes, literal))

    def unapply[VA](value: Value[Nothing, VA]): Option[(VA, Lit[Any])] = value.caseValue match {
      case LiteralCase(attributes, literal) => Some((attributes, literal))
      case _                                => None
    }
    object Raw {
      def apply[A](literal: Lit[A]): RawValue = Literal((), literal)

      def unapply(value: RawValue): Option[Lit[Any]] = value.caseValue match {
        case LiteralCase(_, literal) => Some(literal)
        case _                       => None
      }
    }
  }

  object Reference {
    def apply[A](attributes: A, name: String): Value[Nothing, A] = Value(
      ReferenceCase(attributes, FQName.fromString(name))
    )
    def apply[A](attributes: A, name: FQName): Value[Nothing, A] = Value(ReferenceCase(attributes, name))
    def unapply[A](value: Value[Nothing, A]): Option[(A, FQName)] = value.caseValue match {
      case ReferenceCase(attributes, name) => Some((attributes, name))
      case _                               => None
    }

    object Raw {
      @inline def apply(name: String): RawValue = Reference((), name)
      @inline def apply(name: FQName): RawValue = Reference((), name)
      def unapply(value: Value[Nothing, Any]): Option[FQName] = value.caseValue match {
        case ReferenceCase(_, name) => Some(name)
        case _                      => None
      }
    }
  }

  object Tuple {
    def apply[VA](attributes: VA): Value[Nothing, VA] = Value(TupleCase(attributes, Chunk.empty))

    def apply[TA, VA](attributes: VA, elements: Chunk[Value[TA, VA]]): Value[TA, VA] = Value(
      TupleCase(attributes, elements)
    )

    def apply[TA, VA](attributes: VA, element: Value[TA, VA], otherElements: Value[TA, VA]*): Value[TA, VA] =
      apply(attributes, element +: Chunk.fromIterable(otherElements))

    def unapply[TA, VA](value: Value[TA, VA]): Option[(VA, Chunk[Value[TA, VA]])] = value.caseValue match {
      case TupleCase(attributes, elements) => Some((attributes, elements))
      case _                               => None
    }

    object Raw {
      def apply(elements: Chunk[RawValue]): RawValue = Tuple((), elements)
      def apply(elements: RawValue*): RawValue       = Tuple((), Chunk.fromIterable(elements))

      def unapply(value: RawValue): Option[Chunk[RawValue]] = value.caseValue match {
        case TupleCase(_, elements) => Some(elements)
        case _                      => None
      }
    }
  }

  object Unit {
    def apply[VA](attributes: VA): Value[Nothing, VA] = Value(UnitCase(attributes))

    def unapply[VA](value: Value[Nothing, VA]): Option[VA] = value match {
      case Value(UnitCase(attributes)) => Some(attributes)
      case _                           => None
    }

    object Raw {
      def apply(): RawValue = Value(UnitCase(()))
      def unapply(value: RawValue): Option[scala.Unit] = value match {
        case Value(UnitCase(())) => Some(())
        case _                   => None
      }
    }
  }

  object Variable {
    def apply[VA](attributes: VA, name: Name): Value[Nothing, VA] =
      Value(VariableCase(attributes, name))
    def apply[VA](attributes: VA, name: String): Value[Nothing, VA] =
      Value(VariableCase(attributes, Name.fromString(name)))

    def unapply[VA](value: Value[Nothing, VA]): Option[(VA, Name)] = value.caseValue match {
      case VariableCase(attributes, name) => Some((attributes, name))
      case _                              => None
    }

    object Raw {
      @inline def apply(name: Name): RawValue   = Variable((), name)
      @inline def apply(name: String): RawValue = Variable((), name)
    }
  }
}
