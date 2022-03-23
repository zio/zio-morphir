package zio.morphir.ir.types

import zio.Chunk
import zio.morphir.ir.{AccessControlled, Name}

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
