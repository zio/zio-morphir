package zio.morphir.ir

import zio.morphir.ir.recursive.*
import zio.Chunk

sealed trait Type extends IR { self =>
  import TypeCase.*

  final def asType: Type = self

  override def $case: TypeCase[Type]

  final def fold[Z](f: TypeCase[Z] => Z): Z = self.$case match {
    case c @ ExtensibleRecordCase(_, _) => f(ExtensibleRecordCase(c.name, c.fields.map(_.fold(f))))
    case c @ FieldCase(_, _)            => f(FieldCase(c.name, c.fieldType.fold(f)))
    case c @ FunctionCase(paramTypes, returnType) =>
      f(FunctionCase(paramTypes.map(_.fold(f)), returnType.fold(f)))
    case c @ RecordCase(_) => f(RecordCase(c.fields.map(_.fold(f))))
    case c @ ReferenceCase(typeName, typeParams) =>
      f(ReferenceCase(typeName, typeParams.map(_.fold(f))))
    case c @ TupleCase(_)       => f(TupleCase(c.elementTypes.map(_.fold(f))))
    case c @ UnitCase           => f(UnitCase)
    case c @ VariableCase(name) => f(c)
  }

  /**
   * Folds over the recursive data structure to reduce it to a summary value, providing access to the recursive
   * structure annotated with the current previous summary values in each step of the fold.
   */
  def foldAttributed[Z](f: TypeCase[Attributed[TypeCase, Z]] => Z): Z = {
    def annotate(recursive: Type): Attributed[TypeCase, Z] =
      Attributed(recursive.$case.map(annotate), recursive.foldAttributed(f))
    f($case.map(annotate))
  }
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
    override val $case: TypeCase[Type] = UnitCase
  }

  final case class Field private ($case: FieldCase[Type]) extends Type
  object Field {
    def apply(name: Name, fieldType: Type): Field =
      Field(FieldCase(name, fieldType))
    def unapply(field: Field): Option[(Name, Type)] =
      Some((field.$case.name, field.$case.fieldType))

    object Case {
      def unapply(field: Field): Option[FieldCase[Type]] =
        Some(field.$case)
    }
  }

  final case class Reference(name: FQName, typeParams: Chunk[Type]) extends Type {
    override lazy val $case: ReferenceCase[Type] = ReferenceCase(name, typeParams)
  }

  object Reference {
    object Case {
      def unapply(reference: Reference): Option[ReferenceCase[Type]] =
        Some(reference.$case)
    }
  }

  final case class Variable(name: Name) extends Type {
    override lazy val $case: VariableCase = VariableCase(name)
  }
  object Variable {
    object Case {
      def unapply(variable: Variable): Option[VariableCase] =
        Some(variable.$case)
    }
  }
}
sealed trait Value extends IR { self =>
  import ValueCase.*

  def $case: ValueCase[Value]

  def fold[Z](f: ValueCase[Z] => Z): Z = self.$case match {
    case c @ ApplyCase(_, _)      => f(ApplyCase(c.function.fold(f), c.arguments.map(_.fold(f))))
    case c @ ConstructorCase(_)   => f(c)
    case c @ FieldCase(_, _)      => f(FieldCase(c.target.fold(f), c.name))
    case c @ FieldFunctionCase(_) => f(c)
    case c @ IfThenElseCase(_, _, _) =>
      f(IfThenElseCase(c.condition.fold(f), c.thenBranch.fold(f), c.elseBranch.fold(f)))
    case c @ ListCase(_)    => f(ListCase(c.elements.map(_.fold(f))))
    case c @ LiteralCase(_) => f(c)
    case c @ PatternMatchCase(_, _) =>
      f(
        PatternMatchCase(
          c.branchOutOn.fold(f),
          c.cases.map { case (pattern, value) =>
            (pattern.fold(f), value.fold(f))
          }
        )
      )
    case c @ RecordCase(_)    => f(RecordCase(c.fields.map { case (k, v) => (k, v.fold(f)) }))
    case c @ ReferenceCase(_) => f(c)
    case c @ TupleCase(_)     => f(TupleCase(c.elements.map(_.fold(f))))
    case _ @UnitCase          => f(UnitCase)
    case c @ VariableCase(_)  => f(c)
  }

  /**
   * Folds over the recursive data structure to reduce it to a summary value, providing access to the recursive
   * structure annotated with the current previous summary values in each step of the fold.
   */
  def foldAttributed[Z](f: ValueCase[Attributed[ValueCase, Z]] => Z): Z = {
    def annotate(recursive: Value): Attributed[ValueCase, Z] =
      Attributed(recursive.$case.map(annotate), recursive.foldAttributed(f))
    f($case.map(annotate))
  }
}
object Value {
  import ValueCase.*
  final case class Variable(name: Name) extends Value {
    override lazy val $case: VariableCase = VariableCase(name)
  }
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
  // import IRCase.*
  def $case: IRCase[IR]
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
