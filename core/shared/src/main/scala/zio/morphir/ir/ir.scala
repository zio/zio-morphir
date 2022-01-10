package zio.morphir.ir

sealed trait Type {
  import TypeCase.*
  def $case: TypeCase[Type]
}

object Type {
  import TypeCase.*

  final case class Variable private ($case: VariableCase) extends Type
  object Variable {
    def apply(name: Name): Variable    = Variable(VariableCase(name))
    def unapply(t: Type): Option[Name] = t.$case match {
      case VariableCase(name) => Some(name)
      case _                  => None
    }
  }
}

sealed trait TypeCase[+A] { self =>
  import TypeCase.*
  def map[B](f: A => B): TypeCase[B] = self match {
    case c @ ReferenceCase(_, _) => ReferenceCase(c.typeName, c.typeParams.map(f))
    case c @ TupleCase(_)        => TupleCase(c.elementTypes.map(f))
    case c @ VariableCase(_)     => VariableCase(c.name)
  }
}

object TypeCase {
  final case class ReferenceCase[+A](typeName: FQName, typeParams: List[A]) extends TypeCase[A]
  final case class TupleCase[+A](elementTypes: List[A])                     extends TypeCase[A]
  final case class VariableCase(name: Name)                                 extends TypeCase[Nothing]

}
